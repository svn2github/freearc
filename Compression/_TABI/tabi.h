#include <string.h>
#include <math.h>

// Constants representing various types that may hold TABI_VALUE
enum {TABI_INTEGER=1, TABI_FLOATING=2, TABI_STRING=3, TABI_PTR=4, TABI_FUNCPTR=5};

// Generic function pointer type
typedef void (*VOID_FUNC)();

// Value of unknown type
typedef union _TABI_VALUE
{
	char          placeholder[16];
	long long     int_number;
	double        float_number;
	char*         str;
	void*         ptr;
	VOID_FUNC     funcptr;
} TABI_VALUE;

// Self-tagged and self-typed value
typedef struct _TABI_ELEMENT
{
	char*       name;
	int         type;
	TABI_VALUE  value;
} TABI_ELEMENT;

// Return type of TABI_FUNCTION
typedef int TABI_RESULT_TYPE;

// TABI-style function that accepts variable number of typed tagged parameters
typedef TABI_RESULT_TYPE TABI_FUNCTION(TABI_ELEMENT*);


#ifdef __cplusplus

class TABI_DYNAMAP;

class TABI_MAP
{
	public:
		TABI_MAP(TABI_ELEMENT *params) : p(params) {};
		operator TABI_ELEMENT*() {return p;}

#define TABI_GETTER( TYPE, FIELDTYPE, NAME, TYPECONST, FIELD)                        \
		TYPE NAME(char* n)  {return NAME(n, 0, false);}                      \
		TYPE NAME(char* n, TYPE deflt, bool use_deflt=true)                  \
		{                                                                    \
			TABI_ELEMENT *e = find(n);                                   \
			if (!e) {if (use_deflt)  return deflt;  else throw "";}      \
			switch (e->type)                                             \
			{                                                            \
       				case TYPECONST: return (TYPE)(e->value.FIELD);       \
       				default: throw "";                                   \
       			}                                                            \
		}

		TABI_GETTER( int,             int,             _int,        TABI_INTEGER,   int_number);
		TABI_GETTER( unsigned,        unsigned,        _unsigned,   TABI_INTEGER,   int_number);
		TABI_GETTER( long,            long,            _long,       TABI_INTEGER,   int_number);
		TABI_GETTER( long long,       long long,       _longlong,   TABI_INTEGER,   int_number);
		TABI_GETTER( double,          double,          _double,     TABI_FLOATING,  float_number);
		TABI_GETTER( char*,           char*,           _str,        TABI_STRING,    str);
		TABI_GETTER( const char*,     char*,           _cstr,       TABI_STRING,    str);
		TABI_GETTER( void*,           void*,           _ptr,        TABI_PTR,       ptr);
		TABI_GETTER( TABI_FUNCTION*,  VOID_FUNC,       _callback,   TABI_FUNCPTR,   funcptr);

                // Return value using "return" callback
		template<class T> TABI_RESULT_TYPE _return(T v) {return _callback("return") (TABI_DYNAMAP("result",v));}

                // Dump first n elements (for debugging purposes)
		void dump(int n=0)
		{
			printf("  TABI_MAP: ");
			for (int i=0; i<n?n:100; i++)
			{
				if (p[i].name == NULL)
					break;
				if (i>0)  printf(", ");
				printf("%s: ", p[i].name);
				switch (p[i].type)
				{
					case TABI_STRING:	if (p[i].value.str)   printf("%s", p[i].value.str); break;
					case TABI_INTEGER:	printf("%lld", p[i].value.int_number); break;
					case TABI_FLOATING:	printf("%g", p[i].value.float_number); break;
					case TABI_PTR:	        printf("<%p>", p[i].value.ptr); break;
					case TABI_FUNCPTR:	printf("func<%p>", p[i].value.funcptr); break;
				}
			}
			printf("\n");
		}

                // Memory dump first n elements (for debugging purposes)
		void memory_dump(int n=0)
		{
			printf("dumping TABI_MAP:\n");
			for (int i=0; i<n?n:100; i++)
			{
				for (int j= 0; j< 4; j++)   printf("%02x ", *((unsigned char*)(p+i)+j)); printf("  ");
				for (int j= 4; j< 8; j++)   printf("%02x ", *((unsigned char*)(p+i)+j)); printf("  ");
				for (int j= 8; j<24; j++)   printf("%02x ", *((unsigned char*)(p+i)+j)); printf("  ");
				if (p[i].name == NULL)
					break;
				printf("%10s: ", p[i].name);
				switch (p[i].type)
				{
					case TABI_STRING:	if (p[i].value.str)   printf("%s", p[i].value.str); break;
					case TABI_INTEGER:	printf("%lld", p[i].value.int_number); break;
					case TABI_FLOATING:	printf("%g", p[i].value.float_number); break;
					case TABI_PTR:	        printf("<%p>", p[i].value.ptr); break;
					case TABI_FUNCPTR:	printf("func<%p>", p[i].value.funcptr); break;
				}
				printf("\n");
			}
			printf("\n");
		}

	protected:
		TABI_ELEMENT* p;

	private:

		TABI_ELEMENT* find(char *n)
		{
			for(TABI_ELEMENT* e=p; e->name; e++)
				if (strcmp(e->name,n)==0)
					return e;
                        return NULL;
		}

};


// Constructor for new TABI_MAPs
class TABI_DYNAMAP : public TABI_MAP
{
	public:
		                  TABI_DYNAMAP()             : TABI_MAP(place) {i=0; p[i].name=NULL;}
		template<class T> TABI_DYNAMAP(         T v) : TABI_MAP(place) {i=0; p[i].name=NULL; (*this)("",v);}
		template<class T> TABI_DYNAMAP(char *n, T v) : TABI_MAP(place) {i=0; p[i].name=NULL; (*this)(n,v);}

#define TABI_SETTER( TYPE, FIELDTYPE, NAME, TYPECONST, FIELD)                        \
		TABI_DYNAMAP& operator() (char *n, TYPE v)                           \
		{                                                                    \
			p[i].name = n;                                               \
			p[i].type = TYPECONST;                                       \
			p[i].value.FIELD = (FIELDTYPE)v;                             \
			i++;                                                         \
			p[i].name = NULL;                                            \
			return *this;                                                \
		}

		TABI_SETTER( int,             int,             _int,        TABI_INTEGER,   int_number);
		TABI_SETTER( unsigned,        unsigned,        _unsigned,   TABI_INTEGER,   int_number);
		TABI_SETTER( long,            long,            _long,       TABI_INTEGER,   int_number);
		TABI_SETTER( long long,       long long,       _longlong,   TABI_INTEGER,   int_number);
		TABI_SETTER( double,          double,          _double,     TABI_FLOATING,  float_number);
		TABI_SETTER( char*,           char*,           _str,        TABI_STRING,    str);
		TABI_SETTER( const char*,     char*,           _cstr,       TABI_STRING,    str);
		TABI_SETTER( void*,           void*,           _ptr,        TABI_PTR,       ptr);
		TABI_SETTER( TABI_FUNCTION*,  VOID_FUNC,       _callback,   TABI_FUNCPTR,   funcptr);

	private:
		TABI_ELEMENT place[100];
		int i;
};

#endif // __cplusplus
