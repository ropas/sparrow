typedef struct _charVoid
{
  char charFirst[16];
  int i;
} charVoid;

charVoid str[10][10];


int main(){
  sparrow_print(str);
  f(str[10][20]);
  g(str[30][40].charFirst[50]);
  return 0;
}
