#include <stdio.h>
#include <tabi.h>

extern "C"
{

int server (TABI_ELEMENT* params)
{
	TABI_MAP p(params);  //p.dump(4);
	int             i = p._int("i");               // required parameter
	char           *s = p._str("s", "");           // optional parameter with default value provided
	TABI_FUNCTION *cb = p._callback("callback");   // callback function
	printf("%s %d\n", s, i);
	cb(TABI_DYNAMAP ("n",2) ("name","Bulat"));
	return 0;
}

}
