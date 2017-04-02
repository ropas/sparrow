typedef struct _S
{
  char* seq;
  int n;
} S;

S table[3] = 
  {
    {"a", 1},
    {"b", 2},
    {"c", 3}};


int main(){
  S* p = table;
  sparrow_print(table);
  sparrow_print(p);
  sparrow_print(table[0]);
  sparrow_print(table[0].seq);
  sparrow_print(p->seq);
  sparrow_dump();
  return 0;
}
