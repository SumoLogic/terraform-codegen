package com.sumologic.terraform_generator.writer

import com.sumologic.terraform_generator.StringHelper

case class ProviderFileGenerator(taggedApis: List[String])
  // FIXME: extending a class with null as arg? This seems like a bad idea.
  //  Some of the generator classes extend ScalaTerraformEntity while this one extends
  //  TerraformFileGeneratorBase?
  extends TerraformFileGeneratorBase(null) {

  val functionGenerator = ProviderFunctionGenerator(taggedApis)

  def generate(): String = {
    s"""package sumologic
      |
      |import (
      |	"fmt"
      |	"log"
      |	"net/http"
      |	"os"
      |	"strings"
      |
      |	"github.com/go-errors/errors"
      |	"github.com/hashicorp/terraform-plugin-sdk/helper/mutexkv"
      |	"github.com/hashicorp/terraform-plugin-sdk/helper/schema"
      |	"github.com/hashicorp/terraform-plugin-sdk/terraform"
      |)
      |
      |func Provider() terraform.ResourceProvider {
      |	return &schema.Provider{
      |		Schema: map[string]*schema.Schema{
      |			"access_id": {
      |				Type:        schema.TypeString,
      |				Required:    true,
      |				DefaultFunc: schema.EnvDefaultFunc("SUMOLOGIC_ACCESSID", nil),
      |			},
      |			"access_key": {
      |				Type:        schema.TypeString,
      |				Required:    true,
      |				DefaultFunc: schema.EnvDefaultFunc("SUMOLOGIC_ACCESSKEY", nil),
      |			},
      |			"environment": {
      |				Type:        schema.TypeString,
      |				Optional:    true,
      |				DefaultFunc: schema.EnvDefaultFunc("SUMOLOGIC_ENVIRONMENT", nil),
      |			},
      |			"base_url": {
      |				Type:     schema.TypeString,
      |				Optional: true,
      |				Default:  os.Getenv("SUMOLOGIC_BASE_URL"),
      |			},
      |		},
      |   ${functionGenerator.generateResourcesMap()}
      |   ${functionGenerator.generateDataSourcesMap()}
      |		ConfigureFunc: providerConfigure,
      |	}
      |}
      |
      |var SumoMutexKV = mutexkv.NewMutexKV()
      |
      |func resolveRedirectURL(accessId string, accessKey string) (string, error) {
      |	req, err := http.NewRequest(http.MethodHead, "https://api.sumologic.com/api/v1/collectors", nil)
      |	if err != nil {
      |		return "", err
      |	}
      |	req.SetBasicAuth(accessId, accessKey)
      |	client := &http.Client{CheckRedirect: func(req *http.Request, via []*http.Request) error {
      |		return http.ErrUseLastResponse
      |	}}
      |	resp, err := client.Do(req)
      |	if err != nil {
      |		return "", err
      |	}
      |	location := resp.Header.Get("location")
      |	if location == "" {
      |   // location header not found implies there was no redirect needed
      |   // i.e, the desired environment is us1
      |   return "https://api.sumologic.com/api/", nil
      |	}
      |	return strings.Split(location, "v1")[0], nil
      |}
      |
      |func providerConfigure(d *schema.ResourceData) (interface{}, error) {
      |	accessId := d.Get("access_id").(string)
      |	accessKey := d.Get("access_key").(string)
      |	environment := d.Get("environment").(string)
      |	baseUrl := d.Get("base_url").(string)
      |
      |	msg := ""
      |	if accessId == "" {
      |		msg = "sumologic provider: access_id should be set;"
      |	}
      |
      |	if accessKey == "" {
      |		msg = fmt.Sprintf("%s access_key should be set; ", msg)
      |	}
      |
      |	if environment == "" && baseUrl == "" {
      |		log.Printf("Attempting to resolve redirection URL from access key/id")
      |		url, err := resolveRedirectURL(accessId, accessKey)
      |		if err != nil {
      |			log.Printf("[WARN] Unable to resolve redirection URL, %s", err)
      |			environment = "us2"
      |			// baseUrl will be set accordingly in NewClient constructor
      |			log.Printf("[WARN] environment not set, defaulting to %s", environment)
      |		} else {
      |			baseUrl = url
      |			log.Printf("Resolved redirection URL %s", baseUrl)
      |		}
      |
      |	}
      |
      |	if msg != "" {
      |		return nil, errors.New(msg)
      |	}
      |
      |	return NewClient(
      |		accessId,
      |		accessKey,
      |		environment,
      |		baseUrl,
      |	)
      |}""".stripMargin
  }
}

case class ProviderFunctionGenerator(taggedApis: List[String]) extends StringHelper {

  def generateResourcesMap(): String = {
    val listOfResources = taggedApis.map {
      api =>
        s"""
          |"sumologic_${removeCamelCase(api)}":            resourceSumologic${api.capitalize}(),
          |""".stripMargin
    }.mkString("\n     ")
    s"""
      |   ResourcesMap: map[string]*schema.Resource{
      |			"sumologic_collector":                          resourceSumologicCollector(),
      |			"sumologic_http_source":                        resourceSumologicHTTPSource(),
      |			"sumologic_gcp_source":                         resourceSumologicGCPSource(),
      |			"sumologic_polling_source":                     resourceSumologicPollingSource(),
      |			"sumologic_s3_source":                          resourceSumologicGenericPollingSource(),
      |			"sumologic_s3_audit_source":                    resourceSumologicGenericPollingSource(),
      |			"sumologic_cloudwatch_source":                  resourceSumologicGenericPollingSource(),
      |			"sumologic_aws_inventory_source":               resourceSumologicGenericPollingSource(),
      |			"sumologic_aws_xray_source":                    resourceSumologicGenericPollingSource(),
      |			"sumologic_cloudtrail_source":                  resourceSumologicGenericPollingSource(),
      |			"sumologic_elb_source":                         resourceSumologicGenericPollingSource(),
      |			"sumologic_cloudfront_source":                  resourceSumologicGenericPollingSource(),
      |			"sumologic_cloud_to_cloud_source":              resourceSumologicCloudToCloudSource(),
      |			"sumologic_metadata_source":                    resourceSumologicMetadataSource(),
      |			"sumologic_cloudsyslog_source":                 resourceSumologicCloudsyslogSource(),
      |			"sumologic_user":                               resourceSumologicUser(),
      |			"sumologic_ingest_budget":                      resourceSumologicIngestBudget(),
      |			"sumologic_collector_ingest_budget_assignment": resourceSumologicCollectorIngestBudgetAssignment(),
      |			"sumologic_folder":                             resourceSumologicFolder(),
      |			"sumologic_content":                            resourceSumologicContent(),
      |			"sumologic_partition":                          resourceSumologicPartition(),
      |     "sumologic_connection":                         resourceSumologicConnection(),
      |			"sumologic_monitor":                            resourceSumologicMonitorsLibraryMonitor(),
      |			"sumologic_monitor_folder":                     resourceSumologicMonitorsLibraryFolder(),
      |			"sumologic_ingest_budget_v2":                   resourceSumologicIngestBudgetV2(),
      |			"sumologic_field":                              resourceSumologicField(),
      |			"sumologic_lookup_table":                       resourceSumologicLookupTable(),
      |			"sumologic_subdomain":                          resourceSumologicSubdomain(),
      |			"sumologic_dashboard":                          resourceSumologicDashboard(),
      |			"sumologic_password_policy":                    resourceSumologicPasswordPolicy(),
      |			"sumologic_saml_configuration":                 resourceSumologicSamlConfiguration(),
      |     $listOfResources
      |		},""".stripMargin
  }

  def generateDataSourcesMap(): String = {
    """
      |   DataSourcesMap: map[string]*schema.Resource{
      |			"sumologic_caller_identity": dataSourceSumologicCallerIdentity(),
      |			"sumologic_collector":       dataSourceSumologicCollector(),
      |			"sumologic_http_source":     dataSourceSumologicHTTPSource(),
      |			"sumologic_personal_folder": dataSourceSumologicPersonalFolder(),
      |			"sumologic_my_user_id":      dataSourceSumologicMyUserId(),
      |     "sumologic_role":            dataSourceSumologicRole(),
      |		},""".stripMargin
  }
}
