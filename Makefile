all : main.elm styles

main.elm: frontend/src/* frontend/index.html
	cp frontend/index.html static/index.html
	elm make frontend/src/Main.elm --output=static/main.js

styles: frontend/styles/*
	cp -r frontend/styles static/styles

clean:
	rm -r static/main.js static/styles static/index.html
