function navigate() {
  var agl = robot.GetAngle()%(2*Math.PI);

  if (agl < 0) {
    agl = (2*Math.PI + agl);
  }

  if (ri.GetDistance() > li.GetDistance() && rp.GetDistance() > lp.GetDistance()) {
    dir = 0; 
  } else if (li.GetDistance() > ri.GetDistance() && lp.GetDistance() > rp.GetDistance()) {
    dir = 1; 
  } else if (ri.GetDistance() > lp.GetDistance()) {
    dir = 0; 
  } else if (li.GetDistance() > rp.GetDistance()) {
    dir = 1; 
  }

  if ((mi.GetDistance() >= 3.5 && mi.GetDistance() <= 999)) {
    console.log("forward");  
    move(2, agl); 
  } else if (dir == 1) {
    console.log("left");      
    move(1 + 0.5*Math.random(), agl - Math.PI/20); 
    li.distance = 999;      
  } else if (dir == 0) {
    console.log("right"); 
    move(1 + 0.5*Math.random(), agl + Math.PI/20); 
    ri.distance = 999;      
  }
}
