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
1. Build the image
```
docker build -f web.Dockerfile -t community-connector .
```
2. Push to the approprtiate remote. If using Amazon ECR, instructions can be
found on the dashboard.

3. TODO