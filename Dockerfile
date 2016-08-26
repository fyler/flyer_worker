FROM erlang:19.0.5

ADD . /code/fyler_worker
WORKDIR /code/fyler_worker

RUN useradd -m deplo
RUN chown -R deplo /code/fyler_worker

USER deplo

RUN rebar3 compile
