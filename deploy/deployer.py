import os
import subprocess

from flask import Flask

from dotenv import load_dotenv
from flask_httpauth import HTTPTokenAuth


def create_app():
    load_dotenv()
    app = Flask(__name__)
    auth = HTTPTokenAuth(scheme='Bearer')

    @auth.verify_token
    def verify_token(token):
        return token == os.environ['TOKEN']

    @app.route('/', methods=['POST'])
    @auth.login_required
    def deploy():
        subprocess.run(
            ['git', 'fetch', 'origin'], cwd=os.environ.get('CWD', None)
        )
        subprocess.run(
            ['git', 'checkout', 'origin/gh-pages'],
            cwd=os.environ.get('CWD', None)
        )

    return app
