pipeline {
  agent any
  tools { nodejs "node-8.7.0" }

  stages {
    stage('Prepare') {
      steps {
        script {
          env.EPH_POD_NAME = ephemeralPodName('cb-purs-web3-tests')
        }
      }
    }

    stage('Setup Ephemeral Network') {
      steps {
        createEphemeralPod('cliquebait', 'cb-purs-web3-tests')
        awaitEphemeralPodHttp('cb-purs-web3-tests')
        sh("sed -ie 's/EPH_POD_NAME/${env.EPH_POD_NAME}/g' truffle.js")
      }
    }

    stage('npm install') {
      steps {
        sh('npm install')
      }
    }

    stage('truffle deploy') {
      steps {
        sh("truffle deploy --network ephemeralnet")
      }
    }

    stage('npm run generator'){
      steps {
        sh('npm run generator')
      }
    }

    stage('pulp test') {
      steps {
        sh("NODE_URL=http://${env.EPH_POD_NAME}.geth-pods.jenkins.svc.cluster.local:8545/ pulp test")
      }
    }
  }

  post {
    always {
      sh("echo dumping ephemeral pod logs && kubectl logs -n jenkins ${env.EPH_POD_NAME}")
      teardownEphemeralPod('cb-purs-web3-tests')
    }
  }
}
