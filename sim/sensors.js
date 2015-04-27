function Sensor(id, type, range, phase, spread) {
	this.id = id; 
	this.type = type,
	this.range = range;
	this.phase = phase;
	this.spread = spread; 
	this.distance = 999;
	
	this.GetDistance = function() {
		return this.distance;
	}
	
	this.GetType = function() {
		return this.type;
	}
}