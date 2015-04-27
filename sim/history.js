var errorHistory = [];
var extErrorHistory = [];
var dangHistory = [];

var MAX_IR_DIST = 650;
function finiteIR(input){
  return (input > MAX_IR_DIST) ? MAX_IR_DIST : input;
}

function navigate() {
  var agl = robot.GetAngle()%(2*Math.PI);

  var error = (finiteIR(ri.GetDistance()) - finiteIR(li.GetDistance()));
  var ferror = (MAX_IR_DIST - finiteIR(mi.GetDistance())) / MAX_IR_DIST;

  var ierror = 0;
  for (var i = 0; i < errorHistory.length; ++i) {
    ierror += errorHistory[i];
  }

  var ieerror = 0;
  for (var i = 0; i < extErrorHistory.length; ++i) {
    ieerror += extErrorHistory[i];
  }

  var avgDang = 0;
  for (var i = 0; i < dangHistory.length; ++i) {
    avgDang += dangHistory[i];
  }
  if (avgDang == 0)
    avgDang = 1;

  var derror = 0;
  if (errorHistory.length != 0)
    derror = error - errorHistory[errorHistory.length-1];

  var dang = (0.1 * error + 0.01 * ierror + 0.01 * derror) * (1 + 1 * ferror * avgDang);

  move(3, agl+dang);

  errorHistory.push(error);
  extErrorHistory.push(error);
  dangHistory.push(dang);
  if (errorHistory.length > 10) {
    errorHistory.shift();
  }
  if (dangHistory.length > 10) {
    dangHistory.shift();
  }
  if (extErrorHistory.length > 1000) {
    extErrorHistory.shift();
  }
}
