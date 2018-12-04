FROM tomee:8-jre-7.0.3-plus

COPY . /src
ADD http://nusmv.fbk.eu/distrib/NuSMV-2.6.0-linux64.tar.gz /src
RUN tar xvfz /src/NuSMV-2.6.0-linux64.tar.gz -C /src && ln -s /src/NuSMV-2.6.0-Linux/bin/NuSMV /usr/bin/

WORKDIR /usr/local/tomee/

COPY ./target/smbionet-service-document.war ./webapps/.

EXPOSE 8080