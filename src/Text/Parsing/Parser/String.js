"use strict"

exports._codePointAtIndexUnit = function (just, nothing, i, s) {
  // There is a codePointAt function
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/codePointAt
  // but we're not using it because
  // 1. It seems less supported
  // 2. It returns the CodePoint but doesn't return the information we need
  //    about whether the CodePoint was 1 or 2 CodeUnits.
  // 3. It wastes time checking if the index is at the low unit of a surrogate pair
  // So instead we'll use the charCodeAt function.
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charCodeAt
  let c1 = s.charCodeAt(i);
  if (isNaN(c1)) return nothing; // index is out of bounds
  if (0xDC00 <= c1 && c1 <= 0xDFFF) return nothing; // c1 is the low unit of a surrogate pair
  if (0xD800 <= c1 && c1 <= 0xD8FF) { // c1 is the high unit of a surrogate pair
    let low = s.charCodeAt(i+1); // the low unit of the surrogate pair
    if (isNaN(low)) return nothing; // index is out of bounds
    return just(((c1 - 0xD800) * 0x400) + (low - 0xDC00) + 0x10000, 2);
  }
  return just(c1,1);
}