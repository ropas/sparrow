void* xmalloc1(int size) {
  void* p = malloc(size);
  if (p == 0)
    return xmalloc1(size); // remove this branch when unsound_alloc turns on
  return p;
}

void* xmalloc2(int size) {
  void* p = malloc(size);
  if (p != 0)
    return p;
  return xmalloc2(size);  // remove this when unsound_alloc turns on
}

void* xmalloc3(int size) {
  void* p = malloc(size);
  if (!p)
    return xmalloc3(size); // remove this branch when unsound_alloc turns on
  return p;
}

void* xmalloc4(int size) {
  void* p = malloc(size);
  if (p)
    return p;
  return xmalloc4(size);  // remove this when unsound_alloc turns on
}

int main(){
  void* p1 = xmalloc1(10);
  void* p2 = xmalloc1(20);
  *(p2 + 15) = 1;         // no alarm

  void* p3 = xmalloc2(10);
  void* p4 = xmalloc2(20);
  *(p4 + 15) = 1;         // no alarm

  void* p5 = xmalloc3(10);
  void* p6 = xmalloc3(20);
  *(p6 + 15) = 1;         // no alarm

  void* p7 = xmalloc4(10);
  void* p8 = xmalloc4(20);
  *(p8 + 15) = 1;         // no alarm

  return 0;
}
