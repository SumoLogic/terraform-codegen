# SwaggerToTerraformGenerator

### Running the Generator
From the generator's root directory
./bin/run.sh {Path to aggregate api yml file}  
examples:  
./bin/run.sh ../sumo/external-api/src/main/openapi/schema/dev-api.yml
./bin/run.sh ../sumo/external-api/src/main/openapi/schema/production/external-api.yml

For this to generate anything, at least one api set must be correctly tagged, including:  
- x-tf-{create/update/read/delete}: true  

on each of the respective CRUD endpoints  
and add the following under the aformentioned resource  
FieldExtractionRule
  - x-tf-generated-properties: id,name,scope,parseExpression,enabled  
