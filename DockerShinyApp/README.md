# DockerShinyApp

## Development
To create an RStudio environment preloaded with all dependencies
1. Create a `.env` file that defines the desired RStudio password, e.g.
```
PASSWORD=mystrongpassword
```

2. Start the environment
`docker-compose up`

3. Visit `http://localhost:8787` and start hacking

## Deployment
To create an image ready for hosting the app
1. Build the image. If you need to add commands to to the Dockerfile, be sure to
add `\` at the end of multiline commands.

Also, make sure you're versioning your image. Pick a new version. Look at the repo
in AWS and pick the next version up.

```
docker build -f web.Dockerfile -t community-connector:<version> .
```
2. Sign into the AWS DSE account, and ensure the CLI is fitted to use the DSE
account.

3. Push to the approprtiate remote repo. It is recommended to select another tag
in addition to `latest`, however the application will pull that tag.
```
$(aws ecr get-login --no-include-email --region us-east-1)
docker tag community_connector_app:<version> community_connector_app:latest

docker tag community_connector_app:<version> 059577207107.dkr.ecr.us-east-1.amazonaws.com/community_connector_app:<version>
dokcer push 059577207107.dkr.ecr.us-east-1.amazonaws.com/community_connector_app:<version>

docker tag community_connector_app:latest 059577207107.dkr.ecr.us-east-1.amazonaws.com/community_connector_app:latest
docker push 059577207107.dkr.ecr.us-east-1.amazonaws.com/community_connector_app:latest
```