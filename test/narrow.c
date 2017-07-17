int main(){
  int i;
  int arr[10];
  for(i=0; i < 9; i++)
    arr[i] = 0; // not alarm
  arr[i] = 0;   // not alarm with narrowing

  for(i=0; i < f(); i++)
    arr[i] = 0; // alarm
  return 0;
}
