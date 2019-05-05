# triggering the load with 5 workers

APP_URL="http://localhost:3587"
WORKERS=5
OUT_DIR="output/out3"
RECORDING_LOG="recording/recording2.log"

java -jar bin/shinycannon-1.0.0-b267bad.jar $RECORDING_LOG $APP_URL --workers $WORKERS --output-dir $OUT_DIR

