typedef struct _S
{
  char arr[16];
  int i;
} S;

int main(){
  S str[5];
  str[0].i = 10;
  str[0].arr[20] = 30;

  sparrow_print(str);
  sparrow_print(str[0]);
  sparrow_print(str[0].i);
  sparrow_print(str[0].arr[20]);
  return 0;
}
