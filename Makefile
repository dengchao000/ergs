
PROJECT = gate_server
REBAR = rebar

all: app

deps:
	$(REBAR) get-deps

app: deps
	$(REBAR) compile

clean:
	$(REBAR) clean

clean-doc:
	rm -r -f doc

doc: clean-doc
	$(REBAR) doc skip_deps=true

