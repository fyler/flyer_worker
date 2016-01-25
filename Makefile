ERLANG_ROOT := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)
APPNAME = fyler_worker
APPDIR=$(ERLANG_ROOT)/lib/$(APPNAME)-$(VERSION)
ERL_LIBS:=apps:deps


ERL=erl +A 4 +K true
ifeq (,$(wildcard ./rebar))
	REBAR := $(shell which rebar)
else
	REBAR := ./rebar
endif

ifndef PKG_CONFIG_PATH
	export PKG_CONFIG_PATH = /usr/local/ffmpeg_build/lib/pkgconfig/
endif

all: get-deps update-deps compile

update:
	git pull

get-lager:
	@$(REBAR) -C rebar_lager.config get-deps

get-deps:
	@$(REBAR) get-deps

update-deps:
	@$(REBAR) update-deps

compile:
	@$(REBAR) compile

release: clean compile
	@$(REBAR) generate force=1

soft-release:
	@$(REBAR) generate force=1

clean:
	@$(REBAR) clean

clean-hard:
	rm -rf deps/
	@$(REBAR) clean
	
run:
	ERL_LIBS=apps:deps erl -args_file files/vm.args -sasl errlog_type error -boot  start_sasl -s $(APPNAME) -embedded -config files/app.config

clean-tmp:
	(cd tmp && ls | xargs rm -R)

version:
	echo "VERSION=$(VER)" > version.mk
	git add version.mk
	git commit -m "Version $(VER)"
	git tag -a v$(VER) -m "version $(VER)"
