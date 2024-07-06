#include <assert.h>
#include <stdlib.h>

int cmp(const void *pa, const void *pb)
{
  int a = *(int *)pa;
  int b = *(int *)pb;

  if (a < b) {
    return -1;
  } else if (a == b) {
    return 0;
  } else {
    return 1;
  }
}

void check_array(int *elems, size_t length)
{
  qsort(elems, length, sizeof(int), cmp);

  for (size_t i = 0; i < length - 1; i++) {
    assert(elems[i] <= elems[i + 1]);
  }
}

int main()
{
  {
    int arr[1] = {0};
    check_array(arr, 1);
  }
  {
    int arr[2] = {1, 2};
    check_array(arr, 2);
  }
  {
    int arr[2] = {2, 1};
    check_array(arr, 2);
  }
  {
    int arr[5] = {0, 1, 2, 3, 4};
    check_array(arr, 5);
  }
  {
    int arr[5] = {4, 3, 2, 1, 0};
    check_array(arr, 5);
  }
  {
    int arr[5] = {1, 0, 3, 2, 4};
    check_array(arr, 5);
  }
  {
    int arr[5] = {4, 2, 3, 1, 0};
    check_array(arr, 5);
  }
  {
    int arr[10] = {
        765257874, 933712350,  1354440650, 1856529878, 111694870,
        603621184, 1574814417, 290282169,  441512486,  2061656385,
    };
    check_array(arr, 10);
  }

  return 0;
}
