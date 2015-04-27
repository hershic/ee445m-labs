function loadSimulation() {
  var e = document.getElementById('sim_select');
  var x = document.createElement('script');
  x.src = e.options[e.selectedIndex].value;
  console.log("Loading " + x.src + "...");
  x.onload = startSimulator;
  document.getElementsByTagName("head")[0].appendChild(x);
}
var tRight = [[90, 60],
    [520, 60],
    [520, 210],
    [300, 210],
    [250, 260],
    [250, 320],
    [300, 360],
    [380, 360],
    [510, 410],
    [270, 550],
    [85, 400]];

var tLeft = [[165, 130],
    [450, 130],
    [450, 145],
    [270, 145],
    [190, 245],
    [190, 345],
    [275, 415],
    [380, 415],
    [285, 470],
    [165, 370]];

var b2Vec2 ,b2AABB ,b2BodyDef ,b2Body ,b2FixtureDef ,b2Fixture ,b2World ,b2MassData ,b2PolygonShape 
,b2CircleShape ,b2DebugDraw ,b2MouseJointDef ,b2WeldJointDef, world, sensor, context, fixDef, bodyDef
,robot, robot_fix, i, debugDraw, canvasPosition, bodyList, ctr;
var mouseX, mouseY, mousePVec, isMouseDown, selectedBody, mouseJoint;
var li, ri, mi, lp, rp;

function startSimulator() {         
  b2Vec2 = Box2D.Common.Math.b2Vec2;
  b2AABB = Box2D.Collision.b2AABB;
  b2BodyDef = Box2D.Dynamics.b2BodyDef;
  b2Body = Box2D.Dynamics.b2Body;
  b2FixtureDef = Box2D.Dynamics.b2FixtureDef;
  b2Fixture = Box2D.Dynamics.b2Fixture;
  b2World = Box2D.Dynamics.b2World;
  b2MassData = Box2D.Collision.Shapes.b2MassData;
  b2PolygonShape = Box2D.Collision.Shapes.b2PolygonShape;
  b2CircleShape = Box2D.Collision.Shapes.b2CircleShape;
  b2DebugDraw = Box2D.Dynamics.b2DebugDraw;
  b2MouseJointDef =  Box2D.Dynamics.Joints.b2MouseJointDef;
  b2WeldJointDef =  Box2D.Dynamics.Joints.b2WeldJointDef		;

  world = new b2World(
      new b2Vec2(0, 0)    //gravity
      ,  true                 //allow sleep
      );

  sensors = new Array(); 

  context = document.getElementById("canvas").getContext("2d")

    fixDef = new b2FixtureDef;
  fixDef.density = 1.0;
  fixDef.friction = 0.5;
  fixDef.restitution = 0;

  bodyDef = new b2BodyDef;

  //create ground
  bodyDef.type = b2Body.b2_staticBody;
  fixDef.shape = new b2PolygonShape;
  fixDef.shape.SetAsBox(20, 2);
  bodyDef.position.Set(15, 700 / 30 + 1.8);
  world.CreateBody(bodyDef).CreateFixture(fixDef);
  bodyDef.position.Set(15, -1.8);
  world.CreateBody(bodyDef).CreateFixture(fixDef);
  fixDef.shape.SetAsBox(2, 14);
  bodyDef.position.Set(-1.8, 13);
  world.CreateBody(bodyDef).CreateFixture(fixDef);
  bodyDef.position.Set(35.1, 13);
  world.CreateBody(bodyDef).CreateFixture(fixDef);

  bodyDef.type = b2Body.b2_dynamicBody;
  fixDef.shape = new b2CircleShape(0.4);;
  bodyDef.position.Set(120/30, 80/30);	
  robot = world.CreateBody(bodyDef);
  robot_fix = robot.CreateFixture(fixDef);

  //connect the centers - center in local coordinate - relative to body is 0,0
  //add the joint to the world

  for (i = 0; i < tRight.length; i++) {
    addWall(tRight[i][0], tRight[i][1], tRight[(i+1)%tRight.length][0], tRight[(i+1)%tRight.length][1]); 
  }

  for (i = 0; i < tLeft.length; i++) {
    addWall(tLeft[i][0], tLeft[i][1], tLeft[(i+1)%tLeft.length][0], tLeft[(i+1)%tLeft.length][1]); 
  }

  //addBlob(tLeft); 

  addWall(381, 359, 511, 409);
  addWall(270, 550, 84, 399);
  addWall(511, 411, 271, 551);; 	

  addSensors();

  //setup debug draw
  debugDraw = new b2DebugDraw();
  debugDraw.SetSprite(document.getElementById("canvas").getContext("2d"));
  debugDraw.SetDrawScale(30.0);
  debugDraw.SetFillAlpha(0.5);
  debugDraw.SetLineThickness(1.0);
  debugDraw.SetFlags(b2DebugDraw.e_shapeBit | b2DebugDraw.e_jointBit);
  world.SetDebugDraw(debugDraw);

  window.setInterval(update, 1000 / 240);
  robot.ApplyForce(new b2Vec2(2, 0), robot.GetWorldCenter());

  //mouse
  canvasPosition = getElementPosition(document.getElementById("canvas"));

  document.addEventListener("mousedown", function(e) {
    isMouseDown = true;
    handleMouseMove(e);
    document.addEventListener("mousemove", handleMouseMove, true);
  }, true);

  document.addEventListener("mouseup", function() {
    document.removeEventListener("mousemove", handleMouseMove, true);
    isMouseDown = false;
    mouseX = undefined;
    mouseY = undefined;
  }, true);
}

function handleMouseMove(e) {
  mouseX = (e.clientX - canvasPosition.x) / 30;
  mouseY = (e.clientY - canvasPosition.y) / 30;
};

function getBodyAtMouse() {
  mousePVec = new b2Vec2(mouseX, mouseY);
  var aabb = new b2AABB();
  aabb.lowerBound.Set(mouseX - 0.001, mouseY - 0.001);
  aabb.upperBound.Set(mouseX + 0.001, mouseY + 0.001);

  // Query the world for overlapping shapes.

  selectedBody = null;
  world.QueryAABB(getBodyCB, aabb);
  return selectedBody;
}

function getBodyCB(fixture) {
  if(fixture.GetBody().GetType() != b2Body.b2_staticBody) {
    if(fixture.GetShape().TestPoint(fixture.GetBody().GetTransform(), mousePVec)) {
      selectedBody = fixture.GetBody();
      return false;
    }
  }
  return true;
}

bodyList = new Array(); 
ctr = 0; 

//update
function update() {
  robot.SetAngularVelocity(0); 

  if (ctr > 8) {
    navigate();
    ctr = 0; 
  }

  if (ctr%2 == 0) {
    updateSensors();
  }

  ctr++;

  if(isMouseDown && (!mouseJoint)) {
    var body = getBodyAtMouse();
    if(body) {
      var md = new b2MouseJointDef();
      md.bodyA = world.GetGroundBody();
      md.bodyB = body;
      md.target.Set(mouseX, mouseY);
      md.collideConnected = true;
      md.maxForce = 300.0 * body.GetMass();
      mouseJoint = world.CreateJoint(md);
      body.SetAwake(true);
    }
  }

  if(mouseJoint) {
    if(isMouseDown) {
      mouseJoint.SetTarget(new b2Vec2(mouseX, mouseY));
    } else {
      world.DestroyJoint(mouseJoint);
      mouseJoint = null;
    }
  }

  world.Step(1 / 60, 10, 10);
  world.DrawDebugData();
  world.ClearForces();
};

//helpers

//http://js-tut.aardon.de/js-tut/tutorial/position.html
function getElementPosition(element) {
  var elem=element, tagname="", x=0, y=0;

  while((typeof(elem) == "object") && (typeof(elem.tagName) != "undefined")) {
    y += elem.offsetTop;
    x += elem.offsetLeft;
    tagname = elem.tagName.toUpperCase();

    if(tagname == "BODY")
      elem=0;

    if(typeof(elem) == "object") {
      if(typeof(elem.offsetParent) == "object")
        elem = elem.offsetParent;
    }
  }

  return {x: x, y: y};
}

function addWall(sx, sy, ex, ey) {	  	
  fixDef.shape = new b2PolygonShape;
  bodyDef.type = b2Body.b2_staticBody;
  console.log(b2Body.b2_staticBody);

  fixDef.shape.SetAsArray([
      new b2Vec2(0, 0),
      new b2Vec2((ex - sx)/30, (ey - sy)/30)		
      ]);

  bodyDef.position.x = sx/30;
  bodyDef.position.y = sy/30;

  return world.CreateBody(bodyDef).CreateFixture(fixDef);
}

function addBlob(a) {		
  fixDef.shape = new b2PolygonShape;
  bodyDef.type = b2Body.b2_staticBody;
  console.log(b2Body.b2_staticBody);

  var vertices = new Array();

  var sx = a[0][0]/30;
  var sy = a[0][1]/30; 

  for (i = a.length - 1; i >= 0; i--) {
    vertices.push(new b2Vec2(a[i][0]/30, a[i][1]/30));
  }	

  fixDef.shape.SetAsArray(vertices);

  bodyDef.position.x = sx/30;
  bodyDef.position.y = sy/30;

  return world.CreateBody(bodyDef).CreateFixture(fixDef);		
}

function addSignal(sx, sy, ex, ey) {	  	
  fixDef.shape = new b2PolygonShape;
  fixDef.isSensor = true; 
  bodyDef.type = b2Body.b2_dynamicBody;

  fixDef.shape.SetAsArray([
      new b2Vec2(0, 0),
      new b2Vec2((ex - sx)/30, (ey - sy)/30)
      ]);

  bodyDef.position.x = sx/30;
  bodyDef.position.y = sy/30;

  var body = world.CreateBody(bodyDef);
  body.CreateFixture(fixDef); 

  return body;
}

function updateSensors() {
  for (var i = 0; i < bodyList.length; i++) {
    world.DestroyBody(bodyList[i]); 
  }

  for (var i = 0; i < sensors.length; i++) {
    var b = sensors[i];

    //b.distance = 999; 

    var ax = robot.GetPosition().x;
    var ay = robot.GetPosition().y;
    var bx, by; 
    var angle = robot.GetAngle()%(2*Math.PI);

    //ax = ax - Math.cos(angle)*.4;
    //ay = ay - Math.sin(angle)*.4;

    var subAngle = -(b.spread/3);
    angle = (angle + b.phase)%(2*Math.PI); 

    var finalAngle = angle + subAngle; 
    var newDistance = 999;

    for (var l = 0; l < 3; l++) {
      if (b.spread <= 0) {
        l = 3; 
      }

      if (finalAngle < 0) {
        finalAngle = (2*Math.PI + angle);
      }	

      bx = (b.range/30)*Math.cos(finalAngle) + ax;
      by = (b.range/30)*Math.sin(finalAngle) + ay;

      var p1 = new b2Vec2(ax, ay); 
      var p2 = new b2Vec2(bx, by); 

      var result = world.RayCastAllPoints(p1, p2);

      for (var k = 0; k < result.result.length; k++) {
        if (result.result[k].GetBody().GetType() != b2Body.b2_staticBody) {
          continue;
        }

        var dist = Math.sqrt((result.point[k].x - ax)*(result.point[k].x - ax) + (result.point[k].y - ay)*(result.point[k].y - ay));

        if (dist < newDistance) {
          newDistance = dist; 
        }				
      }

      finalAngle += b.spread/3;
    }

    if (b.distance == 999) {
      b.distance = newDistance;
    } else if (newDistance != 999) {
      b.distance = b.distance + 0.4*(newDistance - b.distance);
    }			

    var bxm = (Math.min(b.distance, b.range/30))*Math.cos(angle) + ax;
    var bym = (Math.min(b.distance, b.range/30))*Math.sin(angle) + ay;

    bodyList.push(addSignal(ax*30, ay*30, bxm*30, bym*30));	
  }	
}

function move(magnitude, angle) {
  var speed = robot.GetLinearVelocity();
  speed = Math.sqrt(speed.x*speed.x + speed.y*speed.y); 

  robot.SetAngle(angle);

  if (speed < 2) {
    robot.SetLinearVelocity(new b2Vec2(magnitude/2*Math.cos(angle), magnitude/2*Math.sin(angle)));
    robot.ApplyForce(new b2Vec2(magnitude*Math.cos(angle), magnitude*Math.sin(angle)), robot.GetWorldCenter());
  } else {
    robot.SetLinearVelocity(new b2Vec2(magnitude*Math.cos(angle), magnitude*Math.sin(angle)));
  }
}

function addSensors() {
  li = new Sensor(0, "left ir", 150, -Math.PI/4, Math.PI/8);
  ri = new Sensor(0, "right ir", 150, Math.PI/4, Math.PI/8);
  mi = new Sensor(0, "middle ir", 200, 0,  Math.PI/16);
  mp = new Sensor(0, "middle ping", 250, 0,  Math.PI/16);
  lp = new Sensor(0, "left ping", 150, -Math.PI/2, Math.PI/6);
  rp = new Sensor(0, "right ping", 150, Math.PI/2, Math.PI/6);

  sensors.push(li);
  sensors.push(ri);
  sensors.push(mi);
  sensors.push(mp);
  sensors.push(lp); 
  sensors.push(rp); 
}
