void* malloc(int size);

int main(){
  int arr[10];
  char* p = malloc(20);
  int i;

  for(i = 0; i < 20; i ++){
    arr[i] = 0;
    *(p + i) = 0;
  }
  return 0;
}

