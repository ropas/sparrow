void* malloc(int size);
typedef struct _S
{
  char arr[16];
  int i;
} S;

int main(){
  S* str = (S*) malloc (sizeof(S));
  str->i = 10;
  str->arr[20] = 30;

  sparrow_print(str);
  sparrow_print(str->i);
  sparrow_print(str->arr[20]);
  return 0;
}
