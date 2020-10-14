quizzer := ~/.local/bin/quizzer

directory := $(PWD)
pidfile := quizzer.pid
lockfile := quizzer.lock

build: readme
	stack install

readme:
	pandoc --standalone --self-contained -M css:README.css -M title:Quizzer README.md -o README.html

install-service: build
	sudo cp quizzer.service /etc/systemd/system/
	sudo chmod 644 /etc/systemd/system/quizzer.service
	sudo systemctl daemon-reload
	sudo systemctl enable quizzer
	
daemon: build kill
	daemonize -c $(directory) -p $(pidfile) -l $(lockfile) $(quizzer)

kill:
	if [ -f $(pidfile) ]; then \
		kill `cat $(pidfile)` && rm -f quizzer.lock quizzer.pid; \
	fi

clean:
	rm README.html quizzer.cabal

.PHONY: build daemon install-service kill readme
