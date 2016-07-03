/// <reference path="./typings/index.d.ts" />
/// <reference path="./node_modules/immutable/dist/immutable.d.ts"/>
'use strict';

import * as minimist from "minimist";
import * as moment from "moment";
import * as React from "react";
import * as ReactDOM from "react-dom";
import Immutable = require('immutable');

interface KeyId<a> {
  [id: string]: a;
};

var items: KeyId<TodoItem> = {};

interface TodoItem {
  name: string,
  created_time: string,
  due: string,
  message: string,
  tags: string[],
  color: string,
  icon: string,
  id: string
};

function strtime(dtime) {
  if (dtime == "") {
    return "";
  } else {
    return moment(dtime).format("YYYY-MM-DD HH:mm:ss");
  }
}

interface TerminalProps {
  hometab: HomeTab,
  actvtab: ActivityTab,
  url: string
};

class Terminal extends React.Component<TerminalProps, {}> {
  ref: any

  constructor() {
    super();
  }

  componentDidMount() {
    let saving_start = () => {
      if ($("#modified").is(":visible") == true) {
        $("#saving-state-ing").show();

        this.save().then(() => {
          $("#modified").hide();
          setTimeout(() => {
            $("#saving-state-ing").hide();
            $("#saving-state-finish").show();

            setTimeout(() => {
              $("#saving-state-finish").fadeOut();
            }, 1000);
          }, 500);
        }).catch(() => { return; });
      }
    };

    setInterval(saving_start, 1000*3.5);
    $(this.ref).focus();
  }

  save = () => {
    let promise_save = (file: string, data: any) => {
      return new Promise((resolve, reject) => {
        $.ajax({
          url: "/save/" + this.props.url + "/" + file,
          type: 'POST',
          data: JSON.stringify(data),
          dataType: 'json',
          contentType: 'application/json'
        })
        .done((d) => resolve())
        .fail((d) => reject());
      });
    };

    return new Promise((resolve, reject) => {
      promise_save('todolist.json', this.props.hometab.state)
        .then(() => {
          promise_save('activity.json', this.props.actvtab.state)
            .then(() => {
              let mp: Immutable.Map<string, TodoItem> = Immutable.Map(items);
              promise_save('items.json', {items: mp.toArray()})
              .then((_) => resolve())
              .catch((_) => reject());
            })
            .catch((_) => reject());
        })
        .catch((_) => reject());
    });
  }

  terminalParser = (str: string) => {
    const args = _.compact(_.split(str, ' '));
    return minimist(args, {
      string: [],
      alias: {
        i: 'icon',
        c: 'color',
        t: 'tags',
        d: 'due',
        m: 'message'
      },
      default: {
        icon: "pencil",
        color: "teal",
        tags: [],
        due: "",
        message: ""
      }
    });
  }

  terminalEvent = (mobj: minimist.ParsedArgs) => {
    if (mobj._.length >= 2 && mobj._[0] == "add") {
      let tags: string[] = _.split(mobj["tags"], ',');
      let now = _.now();
      let item: TodoItem = {
        name: mobj._[1],
        icon: mobj["icon"],
        color: mobj["color"],
        tags: tags,
        due: mobj["due"],
        message: mobj["message"],
        id: now.toString(),
        created_time: strtime(now)
      };
      items[item.id] = item;
      this.props.hometab.addTodo(item.id);
      this.props.actvtab.newActivity("add", [item.id]);
      $(this.ref).val("");
      $("#modified").show();
    } else if (mobj._.length >= 1 && mobj._[0] == "save") {
      this.save();
      $(this.ref).val("");
    } else if (mobj._.length >= 1 && mobj._[0] == "list") {
      if (mobj._[1] == "bytag") {
        this.props.hometab.sortBy((i) => items[i].tags);
      } else if (mobj._[1] == "byicon") {
        this.props.hometab.sortBy((i) => items[i].icon);
      } else if (mobj._[1] == "bycolor") {
        this.props.hometab.sortBy((i) => items[i].color);
      } else if (mobj._[1] == "reverse") {
        this.props.hometab.reverse();
      } else {
        this.props.hometab.sortBy((i) => items[i].created_time);
      }
      $(this.ref).val("");
      $("#modified").show();
    }
  }

  onKeyPress = (event: React.KeyboardEvent) => {
    if (event.key == 'Enter') {
      const com: string = (event.target as HTMLInputElement).value;
      const mobj = this.terminalParser(com);
      this.terminalEvent(mobj);
    }
  }

  render() {
    return (
      <div className="ui inverted transparent fluid left icon input">
        <i className="terminal icon"></i>
        <input
          type="text"
          placeholder="What to do next?"
          onKeyPress={this.onKeyPress}
          ref={(ref) => this.ref = ref}
          />
      </div>
    );
  }
}

interface ItemProps {
  item: TodoItem;
  actvtab: ActivityTab
  checkDone: (string) => void;
  updateItem: (item: TodoItem) => void;
  deleteItem: (string) => void;
};

class Item extends React.Component<ItemProps, {}> {
  accordion: any
  dropdown_icon: any
  dropdown_color: any
  datetime: any

  componentDidMount() {
    ($(this.accordion) as any).accordion();
    ($(this.dropdown_icon) as any).dropdown();
    ($(this.dropdown_color) as any).dropdown();
    ($(this.datetime) as any).datetimepicker({
      format: 'Y-m-d H:i:s',
      step: 30
    });
  }

  _checkDone = () => {
    this.props.checkDone(this.props.item.id);
    this.props.actvtab.newActivity("done", [this.props.item.id]);
  }

  _updateItem = (event) => {
    let item = this.props.item;
    let v = $("#edit-form-" + item.id);
    let name = $(v).find('[name=todo-name]').val();
    let due = $(v).find('[name=due-date]').val();
    let icon = $(v).find('.icon-select').find('.icon-name').text();
    let color = $(v).find('.color-select').find('.color-name').text();
    let message = $(v).find('[name=item-message]').val();
    let tags = $(v).find('[name=item-tags]').val();

    this.props.updateItem($.extend(item, {
      name: name,
      due: due,
      icon: icon,
      color: color,
      message: message,
      tags: _.split(tags, ',')
    }));

    ($(this.accordion) as any).accordion('close', 0);
    this.props.actvtab.newActivity("edit", [this.props.item.id]);
    this.forceUpdate();
  }

  _deleteItem = (event) => {
    this.props.deleteItem(this.props.item.id);
  }

  render() {
    return (
      <div className="event">
        <div className="label">
          <i className={"fitted circular " + this.props.item.color + " " + this.props.item.icon + " icon"}></i>
        </div>
        <div className="content">
          <div className="ui accordion" ref={(ref) => this.accordion = ref}>
            <div className="title" style={{padding: 0}}>
              <div className="summary">
                {this.props.item.name}
                <div className="date timestamp" data-timestamp={this.props.item.created_time}>
                  {moment(this.props.item.created_time).fromNow()}
                </div>
              </div>
            </div>
            <div className="content">
              <form id={"edit-form-" + this.props.item.id} className="edit-form ui small form" onSubmit={ (e) => { e.preventDefault(); } }>
                <div className="field">
                  <label>TODO:</label>
                  <input type="text" name="todo-name" defaultValue={this.props.item.name} />
                </div>
                <div className="field">
                  <label>Message:</label>
                  <textarea name="item-message" defaultValue={this.props.item.message}></textarea>
                </div>
                <div className="two fields">
                  <div className="field">
                    <label>Due:</label>
                    <input type="text" name="due-date" className="datetimepicker" defaultValue={this.props.item.due} ref={(ref) => this.datetime = ref} />
                  </div>
                  <div className="field">
                    <label>Created:</label>
                    <input type="text" name="created-date" defaultValue={this.props.item.created_time} disabled />
                  </div>
                </div>
                <div className="three fields">
                  <div className="field">
                    <label>Icon:</label>
                    <div className="ui top left pointing dropdown basic tiny fluid button" ref={(ref) => this.dropdown_icon = ref}>
                      <span className="text icon-select">
                        <div className="item">
                          <i className="pencil icon"></i> <span className="icon-name">pencil</span>
                        </div>
                      </span>
                      <div className="menu">
                        <div className="ui icon search input" style={{width: "auto"}}>
                          <i className="search icon"></i>
                          <input type="text" placeholder="Search tags..." />
                        </div>
                        <div className="scrolling menu">
                          <div className="item">
                            <i className="pencil icon"></i> <span className="icon-name">pencil</span>
                          </div>
                          <div className="item">
                            <i className="calendar icon"></i> <span className="icon-name">calendar</span>
                          </div>
                          <div className="item">
                            <i className="checkmark icon"></i> <span className="icon-name">checkmark</span>
                          </div>
                          <div className="item">
                            <i className="tasks icon"></i> <span className="icon-name">tasks</span>
                          </div>
                          <div className="item">
                            <i className="alarm icon"></i> <span className="icon-name">alarm</span>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                  <div className="field">
                    <label>Color:</label>
                    <div className="ui top left pointing dropdown basic tiny fluid button" ref={(ref) => this.dropdown_color = ref}>
                      <span className="text color-select">
                        <div className="item">
                          <div className="ui red empty circular label"></div> <span className="color-name">red</span>
                        </div>
                      </span>
                      <div className="menu">
                        <div className="ui icon search input" style={{width: "auto"}}>
                          <i className="search icon"></i>
                          <input type="text" placeholder="Search tags..." />
                        </div>
                        <div className="scrolling menu">
                          <div className="item">
                            <div className="ui red empty circular label"></div> <span className="color-name">red</span>
                          </div>
                          <div className="item">
                            <div className="ui orange empty circular label"></div> <span className="color-name">orange</span>
                          </div>
                          <div className="item">
                            <div className="ui yellow empty circular label"></div> <span className="color-name">yellow</span>
                          </div>
                          <div className="item">
                            <div className="ui olive empty circular label"></div> <span className="color-name">olive</span>
                          </div>
                          <div className="item">
                            <div className="ui green empty circular label"></div> <span className="color-name">green</span>
                          </div>
                          <div className="item">
                            <div className="ui teal empty circular label"></div> <span className="color-name">teal</span>
                          </div>
                          <div className="item">
                            <div className="ui blue empty circular label"></div> <span className="color-name">blue</span>
                          </div>
                          <div className="item">
                            <div className="ui violet empty circular label"></div> <span className="color-name">violet</span>
                          </div>
                          <div className="item">
                            <div className="ui purple empty circular label"></div> <span className="color-name">purple</span>
                          </div>
                          <div className="item">
                            <div className="ui pink empty circular label"></div> <span className="color-name">pink</span>
                          </div>
                          <div className="item">
                            <div className="ui brown empty circular label"></div> <span className="color-name">brown</span>
                          </div>
                          <div className="item">
                            <div className="ui grey empty circular label"></div> <span className="color-name">grey</span>
                          </div>
                          <div className="item">
                            <div className="ui black empty circular label"></div> <span className="color-name">black</span>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                  <div className="field">
                    <label>Tags:</label>
                    <input type="text" name="item-tags" defaultValue={this.props.item.tags} />
                  </div>
                </div>
                <button
                  className="submit ui tiny violet button"
                  type="submit"
                  onClick={this._updateItem}>Submit</button>
                <button
                  className="delete ui tiny button"
                  type="submit"
                  onClick={(e) => {
                    e.preventDefault();
                    this._deleteItem(e);}
                  }>Delete</button>
              </form>
            </div>
            {(() => {
              if ("message" in this.props.item && this.props.item.message.length != 0) {
                <div className="extra text">
                  {this.props.item.message}
                </div>
              }
            })()}
          </div>
          <div className="meta">
            <a className="like">
              <i className="tags icon"></i> {_.join(this.props.item.tags, ', ')}
            </a>
          </div>
          <div
            className="done-button ui vertical animated right floated compact mini button"
            onClick={this._checkDone}>
            <div className="visible content">
              <i className="spinner icon"></i> Undone
            </div>
            <div className="hidden content">
              <i className="check icon"></i> Done?
            </div>
          </div>
        </div>
      </div>
    );
  }
}

interface ItemListProps {
  undones: Immutable.List<string>;
  checkDone: (string) => void;
  actvtab: ActivityTab;
  updateItem: (TodoItem) => void;
  deleteItem: (string) => void;
};

class ItemList extends React.Component<ItemListProps, {}> {
  render() {
    let _props = this.props;
    let itemlist = this.props.undones.map(function(k){
      return (
        <Item {..._props} key={items[k].id} item={items[k]} />
      );
    });

    return (
      <div className="ui feed">{itemlist}</div>
    );
  }
}

interface HomeTabProps {
  url: string;
  actvtab: ActivityTab;
};

interface HomeTabState {
  done: Immutable.List<string>;
  undone: Immutable.List<string>;
  deleted: Immutable.List<string>;
};

function extend<T extends U, U>(itrf: T, part: U): T {
  return $.extend(itrf, part);
}

class HomeTab extends React.Component<HomeTabProps, HomeTabState> {
  constructor() {
    super();
    this.state = {
      done: Immutable.List([]),
      undone: Immutable.List([]),
      deleted: Immutable.List([])
    };
  }

  addTodo = (itemID: string) => {
    this.setState((state, _) => extend(state, {
      undone: state.undone.unshift(itemID)
    }));

    $("#modified").show();
  }

  sortBy = (sf: (string) => any) => {
    this.setState((state, _) => extend(state, {
      undone: state.undone.sortBy(sf)
    }));

    $("#modified").show();
  }

  reverse = () => {
    this.setState((state, _) => extend(state, {
      undone: state.undone.reverse()
    }));

    $("#modified").show();
  }

  checkDone = (itemID: string) => {
    this.setState((state, _) => extend(state, {
      undone: Immutable.List(state.undone).delete(Immutable.List(state.undone).keyOf(itemID)),
      done: state.done.unshift(itemID)
    }));

    $("#modified").show();
  }

  updateItem = (item: TodoItem) => {
    items[item.id] = item;
    $("#modified").show();
  }

  deleteItem = (itemID: string) => {
    this.setState((state, _) => extend(state, {
      undone: Immutable.List(state.undone).delete(Immutable.List(state.undone).keyOf(itemID)),
      deleted: state.deleted.unshift(itemID)
    }));
    $("#modified").show();
  }

  componentDidMount() {
    $.getJSON(this.props.url + '?timestamp=' + _.now(), (j) => {
      this.setState({
        done: j.done,
        undone: j.undone,
        deleted: j.deleted
      });
    });
  }

  render() {
    return (
      <div>
        <h4 className="ui horizontal divider header">
          Todo
        </h4>

        <ItemList
          undones={this.state.undone}
          checkDone={this.checkDone}
          updateItem={this.updateItem}
          deleteItem={this.deleteItem}
          actvtab={this.props.actvtab} />
      </div>
    );
  }
}

type Action = "add" | "edit" | "done"

interface ActivityItem {
  action_time: string,
  action: Action,
  entity: string[]
};

interface MessageProps {
  activity: ActivityItem;
};

class Message extends React.Component<MessageProps, {}> {
  message_pair() {
    if (this.props.activity.action == "add") {
      return ["Add", "add circle", "red"];
    } else if (this.props.activity.action == "edit") {
      return ["Edit", "edit", "violet"];
    } else if (this.props.activity.action == "done") {
      return ["Done", "checkmark", "green"];
    } else {
      console.error("Invalid action: " + this.props.activity.action);
    }
  }

  render() {
    let [label, icon, color] = this.message_pair();

    return (
      <div className={"ui " + color + " horizontal label"}>
        <span><i className={"fitted" + "icon"}></i> {label}</span>
      </div>
    );
  }
}

interface ActivityProps {
  activity: ActivityItem;
};

class Activity extends React.Component<ActivityProps, {}> {
  render() {
    return (
      <div className="event">
        <div className="label">
          <i className={"fitted " + items[this.props.activity.entity[0]].color + " " + items[this.props.activity.entity[0]].icon + " icon"}></i>
        </div>
        <div className="content">
          <div className="summary">
            <Message activity={this.props.activity} />
            {items[this.props.activity.entity[0]].name}

            <div className="date timestamp" data-timestamp={this.props.activity.action_time}>
              {moment(this.props.activity.action_time).fromNow()}
            </div>
          </div>
        </div>
      </div>
    );
  }
}

interface ActivityListProps {
  activities: Immutable.List<ActivityItem>;
};

class ActivityList extends React.Component<ActivityListProps, {}> {
  render() {
    let actvlist = this.props.activities.map(function(actv){
      return (
        <Activity key={actv.action_time} activity={actv} />
      );
    });

    return (
      <div className="ui feed">{actvlist}</div>
    );
  }
}

interface ActivityTabProps {
  url: string;
};

interface ActivityTabState {
  activities: Immutable.List<ActivityItem>;
};

class ActivityTab extends React.Component<ActivityTabProps, ActivityTabState> {
  constructor() {
    super();
    this.state = {
      activities: Immutable.List([])
    };
  }

  newActivity = (act: Action, itemIDs: string[]) => {
    let item: ActivityItem = {
      action: act,
      action_time: strtime(_.now()),
      entity: itemIDs
    };

    this.setState((state, _) => {
      let newState = state;
      newState.activities = newState.activities.unshift(item);
      return newState;
    });
  }

  componentDidMount() {
    $.getJSON(this.props.url + '?timestamp=' + _.now(), (j) => {
      this.setState({
        activities: Immutable.List(j.activities as ActivityItem[])
      });
    });
  }

  render() {
    return (
      <div>
        <h4 className="ui horizontal divider header">
          Activity
        </h4>

        <ActivityList activities={this.state.activities} />
      </div>
    );
  }
}

(function() {
  const tid: string = "20160628192500";
  $.getJSON('todo/' + tid + '/items.json', (j) => {
    _.forEach(j.items, (x: TodoItem) => {
      items[x["id"]] = x;
    });
  })
  .done(() => {
    let actvtab: any = ReactDOM.render(
      <ActivityTab url={'todo/' + tid + '/activity.json'} />,
      document.getElementById('activity-items')
    );

    let hometab: any = ReactDOM.render(
      <HomeTab url={'todo/' + tid + '/todolist.json'} actvtab={actvtab} />,
      document.getElementById('todo-items')
    );

    let terminal: any = ReactDOM.render(
      <Terminal url={tid} hometab={hometab} actvtab={actvtab}/>,
      document.getElementById('terminal')
    );
  });
})();
