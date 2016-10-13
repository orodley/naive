// FLAGS: -fsyntax-only -fdump-tokens

// comment
// comment after other comment
/* 
*/
/*
**/
/***
**/
int /* comment between tokens /* doesn't nest *a**/ foo/**/ =/*/*/ 3;
