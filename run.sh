docker network create app-network
docker build -t "app-db" ./db
docker run --network app-network --name app-db-container -p 5555:5432 -d app-db
stack build
stack exec app-exe 8080 localhost 5555 user pass
curl -i -X POST -H "Content-Type: application/json;charset=utf-8" -d '{"userName":"NAME", "text":"синхрафазатрон в дубне"}' http://localhost:8080/submit
