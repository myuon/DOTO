declare namespace api {
  // activity_list
  declare function getActivities(onSuccess: any, onError: any);
  declare function getActivityByActivity_id(activity_id: any, onSuccess: any, onError: any);
  declare function postActivityNew(body: any, onSuccess: any, onError: any);

  // item
  declare function getItems(onSuccess: any, onError: any);
  declare function getItemsByItem_id(item_id: any, onSuccess: any, onError: any);
  declare function postItemNew(body: any, onSuccess: any, onError: any);
  declare function postItemUpdateByItem_id(item_id, body, onSuccess, onError);

  // list
  declare function getLists(onSuccess: any, onError: any);
  declare function getListByList_id(todolist_id: any, onSuccess: any, onError: any);
  declare function postListNew(body: any, onSuccess: any, onError: any);
  declare function postListUndoneAddByList_idByItem_id(todolist_id: any, todoitem_id: any, onSuccess: any, onError: any);
  declare function getListCheckDoneByList_idByItem_id(list_id: any, item_id: any, onSuccess: any, onError: any);
  declare function postListCheckDeleteByList_idByItem_id(list_id, item_id, onSuccess, onError);

  // user
  declare function getUsers(onSuccess, onError);
  declare function getUserByUser_id(user_id, onSuccess, onError);
  declare function getUser(user_name, onSuccess, onError);
  declare function postUserNew(body, onSuccess, onError);
  declare function postUserUpdateByUser_id(user_id, body, onSuccess, onError);
}

export = api;
