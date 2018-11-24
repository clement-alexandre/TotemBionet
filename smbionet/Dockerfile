FROM openjdk:8

RUN mkdir -p application

COPY ./src ./application
COPY ./pom.xml ./application/pom.xml

WORKDIR /usr/local/

ENTRYPOINT ["java","SmbionetEntryPoint"]