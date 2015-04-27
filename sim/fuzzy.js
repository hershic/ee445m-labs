
	function navigate() {
		var agl = robot.GetAngle()%(2*Math.PI);
		var leftMotor = {val: 0};
		var rightMotor = {val: 0};
		
		if (agl < 0) {
			agl = (2*Math.PI + agl);
		}
		
		SensorInterpret();
		getMotorDiffs(leftMotor, rightMotor);
		/*
		if (ri.GetDistance() > li.GetDistance() && rp.GetDistance() > lp.GetDistance()) {
			dir = 0; 
		} else if (li.GetDistance() > ri.GetDistance() && lp.GetDistance() > rp.GetDistance()) {
			dir = 1; 
		} else if (ri.GetDistance() > lp.GetDistance()) {
			dir = 0; 
		} else if (li.GetDistance() > rp.GetDistance()) {
			dir = 1; 
		}
		*/
		
		var leftAgl =  (leftMotor.val / 1000 * (1*Math.PI/8));
		var rightAgl = -(rightMotor.val / 1000 * (1*Math.PI/8));
		//console.log("leftMotor"+ leftMotor.val);
		//console.log("rightMotor"+ rightMotor.val);
		
		//move(2, agl+leftAgl+rightAgl);
		move(Math.min(leftMotor.val, rightMotor.val)/1000 * 3, agl+leftAgl+rightAgl);
		/*
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
		*/
	}
	
	
	
	
	//fuzzifier
	var MAX_IR_DIST = 650;
	var MAX_PING_DIST = 1300;
	var MAGIC_PING_DIST = 600;

	var frontShort, frontBlocked, 
						leftClose, leftFarther, leftGrowing,
						rightClose, rightGrowing,
						robotFront, robotLeft, robotRight,
						hairPinLeft,
						rightPara, leftPara;
							

	function finiteIR(input){
		return (input > MAX_IR_DIST) ? MAX_IR_DIST : input;
	}
	
	function finitePING(input){
		return (input > MAX_PING_DIST) ? MAX_PING_DIST : input;
	}

	var prevFL=0, prevFR=0;
	var vagueRecollection = 0;
	function SensorInterpret(){
		var shortDist = 400;
		var closeDist = 200;
		var blocked = 150;
		var frontAngle = 0.707106781186 * 100000;
		var temp;
		var index, index2;
		var flIR = finiteIR(li.GetDistance()*200);
		var fcIR = finiteIR(mi.GetDistance()*200);
		var frIR = finiteIR(ri.GetDistance()*200);
		var leftP = finiteIR(lp.GetDistance()*200);
		var rightP = finiteIR(rp.GetDistance()*200);
		var middleP = finiteIR(mp.GetDistance()*200);
		
		

		console.log("flIR: "+flIR+" fcIR: "+fcIR+" frIR: "+frIR+" leftP: "+leftP+" rightP: "+rightP+" middleP: "+middleP);
	  temp = (fcIR > 600) ? 0 : (shortDist / fcIR) * 255;
	  frontShort = (temp > 255) ? 255 : temp;
	  
	  temp = (fcIR > 300) ? 0 : blocked / fcIR;
	  var temp2 = (flIR > 200) ? 0 : blocked / flIR;
	  var temp3 = (frIR > 200) ? 0 : blocked / frIR;
	  temp = Math.max(Math.max((temp) * 255, (temp2) * 255), (temp3) * 255);
	  frontBlocked = (temp > 255) ? 255 : temp;
		
	  //if(flIR > fcIR || frIR > fcIR) frontBlocked = (frontBlocked > 180) ? frontBlocked : 180;
	  
	  temp = (closeDist / (flIR * frontAngle / 100000)) * 255;
		if(flIR >= 400){// && leftP >= MAX_IR_DIST){
			leftClose = 0;
		}else{
			leftClose = (temp > 255) ? 255 : temp;
		}

	  temp = (closeDist / (frIR * frontAngle / 100000)) * 255;
	  if(frIR >= 400){// && rightP >= MAX_IR_DIST){
			rightClose = 0;
		}else{
			rightClose = (temp > 255) ? 255 : temp;
		}

		
	  temp = (flIR - frIR) * 128 * frontAngle / 100000;
	  temp2 = (leftP - rightP) * 128;
	  temp = ((temp*8/8 + temp2*0/8) / MAX_IR_DIST) + 127; /// 0xfff max ir
	  temp = (temp > 255) ? 255 : temp;
	  temp = (temp < 0) ? 0 : temp;
	  //console.log(temp+" : "+frontBlocked);	
	  leftFarther = temp;
	
	if(fcIR <= shortDist && flIR <= shortDist && frIR <= shortDist){
		if(vagueRecollection == 0){
			leftFarther = 255;
		}else{
			leftFarther = 0;
		}
	}else if(fcIR < 500){
		if(frIR > flIR){
			vagueRecollection = 0;
		}else{
			vagueRecollection = 1;
		}
	}
	
	
	temp = (flIR - frIR) * 128 * frontAngle / 100000;
	temp2 = (leftP - rightP) * 128;
	temp = ((temp*2/8 + temp2*6/8) / MAX_PING_DIST) + 128;
	temp = (temp > 255) ? 255 : temp;
	temp = (temp < 0) ? 0 : temp;
	hairPinLeft = temp;

	var threshold = 8;
	/*
	temp = flIR * frontAngle / 100000;
	temp2 = finiteIR(leftP);
	temp = ((temp - temp2) * 255 / (MAX_IR_DIST / threshold));
	temp = (temp < 0) ? 0 : temp;
	//leftPara = temp;	//255 leaving wall, 128 parallel, 0 approaching wall
	//leftGrowing = temp;
	
	temp = frIR * frontAngle / 100000;
	temp2 = finitePING(rightP);
	temp = ((temp - temp2) * 255 / (MAX_IR_DIST / threshold));
	temp = (temp < 0) ? 0 : temp;
	//rightPara = temp;	//255 leaving wall, 128 parallel, 0 approaching wall	
	//rightGrowing = temp;
	*/
	
	temp = (flIR - prevFL) * 255 / (MAX_IR_DIST/threshold);		
	temp = (temp > 255) ? 255 : temp;
	temp = (temp < 0) ? 0 : temp;
		leftGrowing = temp;
		
	prevFL = flIR;
	/*
	if(prevFL == 0){
		prevFL = flIR;
		leftGrowing = 0;
	}else if(flIR <= 400 && first == 1){
		first = 0;
		leftGrowing = 0;
	}else if(temp >= 255/threshold){
		leftGrowing = temp;
		if(first != 2) first = 1;
		prevFL = flIR;
		vagueRecollection = 0;
		//leftFarther = 255;
		//frontBlocked = 230;
		//leftClose = 0;
		
	}
	*/

	temp = (frIR - prevFR) * 255 / (MAX_IR_DIST/threshold);
	temp = (temp > 255) ? 255 : temp;
	temp = (temp < 0) ? 0 : temp;
		rightGrowing = temp;
	prevFR = frIR;
	/*
	if(prevFR == 0){
		prevFR = frIR;
		rightGrowing = 0;
	}else if(frIR <= 400 && first == 2){
		first = 0;
		rightGrowing = 0;
	}else if(temp >= 255/threshold){
		rightGrowing = temp;
		if(first != 1) first = 2;
		prevFR = frIR;
		vagueRecollection = 1;
		//leftFarther = 0;
		//frontBlocked = 230;
		//rightClose = 0;
	  
	}
	*/
	/*
	if(first == 1){
		leftGrowing = 0;
	}else if (first == 2){
		rightGrowing = 0;
	}
	*/
/*
	if(frontBlocked >= 190){
		if(vagueRecollection == 0){
			leftFarther = 255;
			//leftClose = 0;
			first = 0;
		}else{
			leftFarther = 0;
			//rightClose = 0;
			first = 0;
		}
	}
	*/
  	console.log("frontShort: "+frontShort+" frontBlocked: "+frontBlocked+" leftClose: "+leftClose+" rightClose: "+rightClose+" leftFarther: "+leftFarther+" leftGrowing: "+leftGrowing+" rightGrowing: "+rightGrowing);

	  robotFront = 0;
	  robotLeft = 0;
	  robotRight = 0;
	}
	
	function getMotorDiffs(leftMotor, rightMotor){
		FUZZY_next(leftMotor, rightMotor);
	}
	
	
	
	//fuzzy
	var LOGIC_MAX = 255
	var RANGE = 2000
	//#define MIN_SPEED -1000
	var MAX_SPEED = 1000
	var MAX_CHANGE = 1000 


	function NOT(x){
		return 255 - x;
	}

	function AND(x, y){
		return (x > y) ? y : x;
	}

	function OR(x, y){
		return (x > y) ? x : y;
	}

	function softTurn(frontShort, frontClear,
							leftClose, leftFarther, leftGrowing,
							rightClose,
							robotLeft){
								
		return OR(AND(frontShort, leftFarther),			
					OR(
						
						AND(NOT(leftClose), leftGrowing),
						//leftGrowing,
						AND(rightClose, OR(NOT(leftClose), AND(leftClose, robotLeft)))
					)
				);
	}
							
	function hardTurn(frontBlocked, robotFront, leftFarther, leftGrowing){
		var temp;
		if(leftFarther > 255/2){
			temp = 255*3/2 - leftFarther;
		}else{
			temp = 255/2 - leftFarther;
		}
		return leftFarther;
	}

	function FUZZY_next(leftMotor, rightMotor){
		
		var frontClear = (NOT(frontShort) && NOT(frontBlocked)),
			//leftGood = NOT(leftClose),
			//rightGood = NOT(rightClose),
			rightFarther = NOT(leftFarther);
			//leftShrinking = rightGrowing,
			//rightShrinking = leftGrowing;
		
		var softLeft, softRight, hardLeft, hardRight;
		var temp;
		
		softLeft = softTurn(frontShort, frontClear,
							leftClose, leftFarther, leftGrowing,
							rightClose,
							robotLeft);
		
		
		softRight = softTurn(frontShort, frontClear,
							rightClose, rightFarther, rightGrowing,
							leftClose,
							robotRight);
							
		//console.log("softLeft: "+softLeft+" softRight: "+softRight);
		
		hardLeft = hardTurn(frontBlocked, robotFront, leftFarther, leftGrowing);
		//console.log("hardLeft "+hardLeft);
		
		hardRight = hardTurn(frontBlocked, robotFront, rightFarther, rightGrowing);
		//console.log("hardRight "+hardRight);
		
		if((frontBlocked >= 230) && robotFront < 255*1/2){
			if(frontBlocked == 255) temp = -200;
			else temp = 100;
			leftMotor.val = temp;
			rightMotor.val = temp;
		
			temp = ((NOT(leftFarther) - leftFarther) * MAX_CHANGE)/255;
			//temp = ((hardRight - hardLeft) * MAX_CHANGE)/255;
			//temp = ((softRight - softLeft) * MAX_CHANGE)/255;
			leftMotor.val += temp;
			rightMotor.val -= temp;
		}else{
			temp = 500 + 500 * NOT(frontBlocked) / 255;
			leftMotor.val = temp;
			rightMotor.val = temp;
			
			temp = ((softRight - softLeft) * MAX_CHANGE/2)/255;

			leftMotor.val += temp;
			rightMotor.val -= temp;
		}
	
		leftMotor.val = (leftMotor.val > 1000) ? 1000 : leftMotor.val;
		leftMotor.val = (leftMotor.val < -1000) ? -1000 : leftMotor.val;
		rightMotor.val = (rightMotor.val > 1000) ? 1000 : rightMotor.val;
		rightMotor.val = (rightMotor.val < -1000) ? -1000 : rightMotor.val;
	}

