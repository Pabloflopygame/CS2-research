
.PHONY: setup seeding bfs owned inventory buff join all clean

setup:
	Rscript scripts/00_setup.R

seeding:
	Rscript scripts/01_seeding.R

bfs:
	Rscript scripts/02_bfs.R

owned:
	Rscript scripts/03_owned_games.R

inventory:
	Rscript scripts/04_inventory.R

buff:
	Rscript scripts/05_buff.R

join:
	Rscript scripts/06_join.R

all: setup seeding bfs owned inventory buff join

clean:
	rm -f out/*.csv out/*.rds
