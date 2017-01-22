# Distributed File System

Name: Kris Vanhoutte

Student Number: 12301975

## Setup Instructions

A setup file has been provided which downloads the necessary libraries as well as the content of this github. The steps involved are:

1. If using docker, `run sudo docker run -i -t ubuntu /bin/bash` to start a docker container. All following steps should be run inside the docker container, or in a terminal window if not using docker.

2. Next run:
  ```
  $ apt-get update && apt-get install -y wget
  $ wget https://raw.githubusercontent.com/vanhoutk/DistributedFileSystem/master/setup.sh
  ```
  This will download the setup script.

3. Run the script by using
  ```
  $ chmod +x setup.sh
  $ source setup.sh
  ```

Once the libraries have been installed, the distributed file system can be built using the build.sh file, and then run with the run.sh file.
A number of issues were encountered during testing, two of which can be resolved as follows:

  1. Some versions of packages might result in build errors:
    - To solve remove the .stack-work folder (`rm -rf .stack-work`) for the affected application and rebuild

  2. The client build might throw up a gtk2hsSetup error message. To solve run:
    ```
    $ stack build --resolver lts-7.16
    $ stack install alex happy gtk2hs-buildtools --resolver lts-7.16
    ```