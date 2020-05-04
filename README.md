# SwaggerToTerraformGenerator
Terraform support for OpenAPI compliant APIs with some restrictions (see [What types of APIs are supported?](#what-types-of-apis-are-supported) section)


## What types of APIs are supported?
Currently, we’re able to _only_ support APIs that follow the CRUD model (for example Users and Roles API). With these CRUD endpoints, it is straightforward for Terraform to manage the lifecycle of our resources. We’re not currently able to support APIs that don’t follow the CRUD model.


## Building the Generator
Run the following command from the root directory to build the generator. 
```
./mvnw clean compile
```

## Running the Generator
Use `run.sh` under `bin` directory to run the generator. The usage is
```bash
./bin/run.sh {Path to a valid yaml file}  
```
In our case, the yaml file would be either `external-api/src/main/openapi/schema/dev-api.yml` or `external-api/src/main/openapi/schema/production/external-api.yml`.

Examples:
1. Run generator for public APIs (beta/GA).
  ```
  ./bin/run.sh ../sumo/external-api/src/main/openapi/schema/production/external-api.yml
  ```
2. Run generator for alpha APIs
  ```bash
  ./bin/run.sh ../sumo/external-api/src/main/openapi/schema/dev-api.yml
  ```

_For this to generate anything, at least one API must be correctly tagged_. See [How to enable the Generator for your API?](#how-to-enable-the-generator-for-your-api) for details.


## How to enable the Generator for your API?
Your API must be have following OpenAPI vendor extensions.

1. `x-tf-create` (boolean value):
The create endpoint for this API. This tag goes in the path section of the yaml file under the operation header for an endpoint.

2. `x-tf-read` (boolean value):
The read endpoint for this API. This tag goes in the path section of the yaml file under the operation header for an endpoint.

3. `x-tf-update` (boolean value):
The update endpoint for this API. This tag goes in the path section of the yaml file under the operation header for an endpoint.

4. `x-tf-delete` (boolean value):
The delete endpoint for this API. This tag goes in the path section of the yaml file under the operation header for an endpoint.

5. `x-tf-resource-name` (string value) (OPTIONAL):
This is the name that we want the resource to have. This is important in the case that the component name doesn’t match the name we want to show users. If not used, then the resource name will default to the component name.

6. `x-tf-generated-properties` (string value):
Once you’ve chosen the model that you want to use as your resource (e.g. ExtractionRule for extraction-rule-management.yml), this tag is used to specify which fields we want to generate in the provider (i.e. allow users to set). This will be a comma-separated list of field names. When choosing the model, make sure to choose a model that either inherits the models that are used for create and update requests, or contains the union of the fields of both of those models. This is because that model (and the models it inherits, if any) will need to contain the fields that we specify in this tag, so that we can make any CRUD API call. 


As an example, look at [role-management.yml](https://github.com/Sanyaku/sumologic/blob/master/external-api/src/main/openapi/schema/role-management.yml) file.

