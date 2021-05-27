FROM asia.gcr.io/datamusterapp/datamuster/rbase



RUN R -e "install.packages('lubridate')"

# copy the app to the image
RUN mkdir /root/DataMusterApp
COPY Webapp /root/DataMuster48h



 

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/DataMuster48h')"]