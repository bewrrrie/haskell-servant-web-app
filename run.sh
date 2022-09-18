docker network create app-network
docker build -t "app-db" ./db
docker run --network app-network --name app-db-container -p 5555:5432 -d app-db
stack build
stack exec app-exe 8080 10 10 localhost 5555 user pass
