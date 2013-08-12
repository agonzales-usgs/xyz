/* Simple routines for interfacing Fortran and C. */
/* Feb. 19, 2013 - while porting to Linux, discovered that the code that
 * re-terminates the string was writing one byte beyond the allocated memory.
 */

int white(k)
char k;
/* Tells if it's whitespace or not. */
{
  switch(k) {
    case ' ':
    case '\n':
    case '\t': return(1);
  }
  return(0);
} /* white */

char *ftocstring(fstring, len)
char *fstring;
long len;
{
  char *ret;
  int i;

  ret= (char *)malloc( sizeof(char) * (len+1) );
  if( ret== NULL )
    return(NULL);
  memcpy(ret,fstring, sizeof(char)*len);
  /* Feb. 19, 2013 - changed ret[len]=0 to ret[n] = '\0' */
  ret[len] = '\0';

  /* Now we look for the last non-whitespace character to
     re-terminate the string. */
  for(i=len; white(ret[i]) && i; i--) 
    ;
  /* Feb. 19, 2013 - changed ret[i+1]=0 to ret[i] = '\0' */
  ret[i] = '\0';
;
  return(ret);
} /* cstring */

