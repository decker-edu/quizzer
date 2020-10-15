
build: readme
	stack install

readme:
	pandoc --standalone --self-contained -M css:README.css -M title:Quizzer README.md -o README.html

install-service: build
	sudo cp quizzer.service /etc/systemd/system/
	sudo chmod 644 /etc/systemd/system/quizzer.service
	sudo systemctl daemon-reload
	sudo systemctl enable quizzer
	
run-local:
	(sleep 1; open http://localhost:3003/presenter.html)&
	stack run -- quizzer -d

clean:
	rm README.html quizzer.cabal

.PHONY: build daemon install-service kill readme
