int arr[10];

int main(){
  int* p = arr;
  *(p+1) = 10;
  sparrow_print(arr);
  sparrow_print(p);
  sparrow_print(*p);

  int larr[10];
  larr[1] = 20;
  sparrow_print(larr);
  sparrow_print(larr[1]);
  return 0;
}
