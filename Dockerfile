# load existing image
FROM rocker/tidyverse:4.0.0

# install packages needed
RUN R -e "install.packages('ggsci')"
RUN R -e "install.packages('tidytext')"
RUN R -e "install.packages('scales')"

# copy the necessary files from the folder into the image
COPY /analise_receitas_eleitorais_senadores.R /analise_receitas_eleitorais_senadores.R
COPY /input/dados-eleitorais.rda /input/dados-eleitorais.rda
COPY /input/macroregioes.xlsx /input/macroregioes.xlsx
COPY /output /output

# run the R script
CMD Rscript /analise_receitas_eleitorais_senadores.R