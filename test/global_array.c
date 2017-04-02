char *longstats[5] = { "1", "2", "3", "4", "5" };

int main(){

  char** p1 = longstats;
  
  sparrow_print(p1);
  char* p2 = *p1;
  sparrow_print(p2);
  return 0;
}
