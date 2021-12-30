/* -*- mode: groovy -*-
  Confgure how to run our job in Jenkins.
  See https://castle-engine.io/cloud_builds_jenkins .
*/

pipeline {
  agent {
    docker {
      image 'kambi/castle-engine-cloud-builds-tools:cge-unstable'
    }
  }
  stages {
    stage('Code Samples') {
      steps {
        sh 'cd htdocs/code-samples/ && make'
      }
    }
  }
  post {
    regression {
      mail to: 'michalis@castle-engine.io',
        subject: "[jenkins] Build started failing: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    failure {
      mail to: 'michalis@castle-engine.io',
        subject: "[jenkins] Build failed: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    fixed {
      mail to: 'michalis@castle-engine.io',
        subject: "[jenkins] Build is again successful: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
  }
}
