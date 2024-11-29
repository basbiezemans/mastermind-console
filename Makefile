build:
	docker build -t mastermind .

run:
	docker run -it mastermind

shell:
	docker run -it mastermind sh

# CLEANUP
# -a, --all		Remove all unused images not just dangling ones
# -f, --force	Do not prompt for confirmation
# --volumes		Prune anonymous volumes
cleanup:
	docker system prune -a -f --volumes

docker:
	docker build -t mastermind . && docker run -it mastermind