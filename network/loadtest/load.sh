# triggering the load with 5 workers

APP_URL="http://localhost:7897"
WORKERS=5
OUT_DIR="out"

java -jar shinycannon-1.0.0-b267bad.jar recording.log $APP_URL --workers $WORKERS --output-dir $OUT_DIR

