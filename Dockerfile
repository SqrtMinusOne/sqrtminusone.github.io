FROM nginx:1.21.4
RUN apt-get update && apt-get install -y python3 python3-pip git
RUN pip install flask python-dotenv flask_httpauth gunicorn
COPY . /usr/share/nginx/html
RUN mv /usr/share/nginx/html/deploy /
RUN (cd /usr/share/nginx/html && git remote set-url origin https://github.com/SqrtMinusOne/sqrtminusone.github.io.git && git reset origin/master --hard)
RUN rm /usr/share/nginx/html/index.html
RUN mv /deploy/default.conf /etc/nginx/conf.d/default.conf
CMD bash /deploy/entrypoint.sh
