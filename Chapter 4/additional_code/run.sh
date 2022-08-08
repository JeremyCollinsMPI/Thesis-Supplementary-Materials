# docker build -t pal .
docker run -it --rm -v $PWD:/src pal python main.py
