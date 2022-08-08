#docker build -t mtdna .
docker run -d --rm -v $PWD:/src mtdna bash entrypoint.sh
