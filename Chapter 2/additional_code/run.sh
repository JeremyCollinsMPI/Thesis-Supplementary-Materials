# docker build -t phonotactics .
docker run -it --rm -v $PWD:/src phonotactics python main.py
