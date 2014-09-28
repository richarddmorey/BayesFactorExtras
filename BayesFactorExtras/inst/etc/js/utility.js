function expString(x)
{
  if( isNaN(x) ) return "NA";
  
  var toBase10log = x / Math.log(10);
  var numMax = Math.log(Number.MAX_VALUE);
  var numMin = Math.log(Number.MIN_VALUE);

  var first, second;
   

  if( x > numMax ){
    first = Math.pow( 10, toBase10log - Math.floor(toBase10log) );
    second = Math.floor(toBase10log);
    return first + "e+" + second;
  }else if( x < numMin ){
    first = Math.pow( 10, 1 - (Math.ceil(toBase10log) - toBase10log) );
    second = Math.ceil(toBase10log) - 1;
    return first + "e" + second;    
  }else{
    return Math.exp(x);
  }
}
