
build: readme
	stack install

readme:
	pandoc --standalone --self-contained -M css:README.css -M title:Quizzer README.md -o README.html

install-service: build
	sudo cp ~/.local/bin/quizzer /usr/local/bin
	sudo cp quizzer.service /etc/systemd/system/
	sudo chmod 644 /etc/systemd/system/quizzer.service
	sudo systemctl daemon-reload
	sudo systemctl enable quizzer
	sudo mkdir -p /var/log/quizzer /var/tmp/quizzer
	sudo chown daemon:daemon /var/log/quizzer /var/tmp/quizzer
	sudo systemctl restart quizzer
	sudo systemctl status quizzer
	
run-local:
	(sleep 1; open http://localhost:3003/presenter.html)&
	stack run -- quizzer -d

clean:
	rm README.html quizzer.cabal

.PHONY: build daemon install-service kill readme
