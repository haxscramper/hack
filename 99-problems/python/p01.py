from typing import List, Optional, Generic, TypeVar

T = TypeVar('T')


def last_item(list: List[T]) -> Optional[T]:
    if len(list) > 0:
        return list[-1]


print(last_item([0,2,3,4]))
print(last_item([]))
