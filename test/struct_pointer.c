void* malloc(int size);
typedef struct _S
{
  char arr[16];
  int i;
} S;

int main(){
  S str;
  S* p1 = &str;
  S* p2 = (S*) malloc(sizeof(S));
  p1->i = 100;
  p2->i = 200;
  sparrow_print(str);
  sparrow_print(p1);
  sparrow_print(p2);
  sparrow_print(str.i);
  sparrow_print(p1->i);
  sparrow_print(p2->i);
  return 0;
}
