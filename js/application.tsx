'use strict';

import * as minimist from "minimist";
import * as moment from "moment";
import * as React from "react";
import * as ReactDOM from "react-dom";
import * as _ from "lodash";
import * as $ from "jquery";

function extend<T extends U, U>(itrf: T, part: U): T {
  return $.extend(itrf, part);
}

var items: Map<String, TodoItem> = new Map();

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

interface TerminalState {
  history: string[],
  historyIndex: number
};

class Terminal extends React.Component<TerminalProps, TerminalState> {
  ref: any

  constructor() {
    super();
    this.state = {history: [], historyIndex: -1}
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

    let todolist: HomeTabState = this.props.hometab.state;
    let activity: ActivityTabState = this.props.actvtab.state;

    return new Promise((resolve, reject) => {
      promise_save('todolist.json', todolist)
        .then(() => {
          promise_save('activity.json', activity)
            .then(() => {
              promise_save('items.json', {items: items.values})
              .then((x) => resolve())
              .catch((x) => reject());
            })
            .catch((x) => reject());
        })
        .catch((x) => reject());
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

  onKeyUp = (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key == 'Enter') {
      const text: string = (event.target as HTMLInputElement).value;
      const mobj = this.terminalParser(text);
      if (text != "") {
        this.setState((state, s) => extend(state, {
          history: _.concat([text], state.history)
        }));
      }

      this.terminalEvent(mobj);
    }

    // arrowkey up
    if (event.keyCode == 38) {
      if (this.state.historyIndex != this.state.history.length - 1) {
        this.setState((state, _) => extend(state, {
          historyIndex: state.historyIndex + 1
        }));
      }
      if (this.state.historyIndex != -1) {
        $(this.ref).val(this.state.history[this.state.historyIndex]);
      } else {
        $(this.ref).val("");
      }
    }
    // arrowkey down
    if (event.keyCode == 40) {
      if (this.state.historyIndex != -1) {
        this.setState((state, _) => extend(state, {
          historyIndex: state.historyIndex - 1
        }));
        $(this.ref).val(this.state.history[this.state.historyIndex]);
      }
      else { $(this.ref).val(""); }
    }
  }

  render() {
    return (
      <div className="ui inverted transparent fluid left icon input">
        <i className="terminal icon"></i>
        <input
          type="text"
          placeholder="What to do next?"
          onKeyUp={this.onKeyUp}
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
    // 一旦
//    ($('.ui.accordion') as any).accordion();
//    ($(this.dropdown_icon) as any).dropdown('set selected', this.props.item.icon);
//    ($(this.dropdown_color) as any).dropdown('set selected', this.props.item.color);
//    ($(this.datetime) as any).datetimepicker({
//      format: 'Y-m-d H:i:s',
//      step: 30
//    });
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
    let icon = ($(this.dropdown_icon) as any).dropdown('get value');
    let color = ($(this.dropdown_color) as any).dropdown('get value');
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
                      </span>
                      <div className="menu">
                        <div className="ui icon search input" style={{width: "auto"}}>
                          <i className="search icon"></i>
                          <input type="text" placeholder="Search tags..." />
                        </div>
                        <div className="scrolling menu">
                          <div className="item" data-value="pencil">
                            <i className="pencil icon"></i> pencil
                          </div>
                          <div className="item" data-value="calendar">
                            <i className="calendar icon"></i> calendar
                          </div>
                          <div className="item" data-value="checkmark">
                            <i className="checkmark icon"></i> checkmark
                          </div>
                          <div className="item" data-value="tasks">
                            <i className="tasks icon"></i> tasks
                          </div>
                          <div className="item" data-value="alarm">
                            <i className="alarm icon"></i> alarm
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
                        </div>
                      </span>
                      <div className="menu">
                        <div className="ui icon search input" style={{width: "auto"}}>
                          <i className="search icon"></i>
                          <input type="text" placeholder="Search tags..." />
                        </div>
                        <div className="scrolling menu">
                          <div className="item" data-value="red">
                            <div className="ui red empty circular label"></div> red
                          </div>
                          <div className="item" data-value="orange">
                            <div className="ui orange empty circular label"></div> orange
                          </div>
                          <div className="item" data-value="yellow">
                            <div className="ui yellow empty circular label"></div> yellow
                          </div>
                          <div className="item" data-value="olive">
                            <div className="ui olive empty circular label"></div> olive
                          </div>
                          <div className="item" data-value="green">
                            <div className="ui green empty circular label"></div> green
                          </div>
                          <div className="item" data-value="teal">
                            <div className="ui teal empty circular label"></div> teal
                          </div>
                          <div className="item" data-value="blue">
                            <div className="ui blue empty circular label"></div> blue
                          </div>
                          <div className="item" data-value="violet">
                            <div className="ui violet empty circular label"></div> violet
                          </div>
                          <div className="item" data-value="purple">
                            <div className="ui purple empty circular label"></div> purple
                          </div>
                          <div className="item" data-value="pink">
                            <div className="ui pink empty circular label"></div> pink
                          </div>
                          <div className="item" data-value="brown">
                            <div className="ui brown empty circular label"></div> brown
                          </div>
                          <div className="item" data-value="grey">
                            <div className="ui grey empty circular label"></div> grey
                          </div>
                          <div className="item" data-value="black">
                            <div className="ui black empty circular label"></div> black
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
              if (this.props.item.message !== undefined && this.props.item.message.length != 0) {
                return (
                  <div className="extra text">
                    {this.props.item.message}
                  </div>
                );
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
  undones: string[];
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
  done: string[];
  undone: string[];
  deleted: string[];
};

class HomeTab extends React.Component<HomeTabProps, HomeTabState> {
  constructor() {
    super();
    this.state = {
      done: [],
      undone: [],
      deleted: []
    };
  }

  addTodo = (itemID: string) => {
    this.setState((state, _) => {
      state.undone.unshift(itemID);
      return state
      });
    $("#modified").show();
  }

  sortBy = (sf: (string) => any) => {
    this.setState((state, _) => {
      state.undone.sort(sf);
      return state;
    });
    $("#modified").show();
  }

  reverse = () => {
    this.setState((state, _) => {
      state.undone.reverse();
      return state;
    });
    $("#modified").show();
  }

  checkDone = (itemID: string) => {
    this.setState((state, s) => {
      return extend(state, {
        undone: _.pull(state.undone, itemID),
        done: _.concat([itemID], state.done)
      });
    });
    $("#modified").show();
  }

  updateItem = (item: TodoItem) => {
    items[item.id] = item;
    $("#modified").show();
  }

  deleteItem = (itemID: string) => {
    this.setState((state, s) => {
      return extend(state, {
        undone: _.pull(state.undone, itemID),
        deleted: _.concat([itemID], state.deleted)
      });
    });
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
    let hasdue = (v) => "due" in items[v] && items[v].due != ""
    let inaday = (v) => moment(items[v].due) < moment().endOf('day');
    let inaweek = (v) => moment(items[v].due) < moment().add(1, 'week');
    return (
      <div>
        {status}

        <h4 className="ui horizontal divider header">
          Today
        </h4>

        <ItemList
          undones={this.state.undone.filter((v) => hasdue(v) && inaday(v))}
          checkDone={this.checkDone}
          updateItem={this.updateItem}
          deleteItem={this.deleteItem}
          actvtab={this.props.actvtab} />

        <h4 className="ui horizontal divider header">
          This Week
        </h4>

        <ItemList
          undones={this.state.undone.filter((v) => hasdue(v) && !inaday(v) && inaweek(v))}
          checkDone={this.checkDone}
          updateItem={this.updateItem}
          deleteItem={this.deleteItem}
          actvtab={this.props.actvtab} />

        <h4 className="ui horizontal divider header">
          Others
        </h4>

        <ItemList
          undones={this.state.undone.filter((v) => hasdue(v) && !inaday(v) && !inaweek(v))}
          checkDone={this.checkDone}
          updateItem={this.updateItem}
          deleteItem={this.deleteItem}
          actvtab={this.props.actvtab} />

        <h4 className="ui horizontal divider header">
          Todo
        </h4>

        <ItemList
          undones={this.state.undone.filter((v) => !hasdue(v))}
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
  activities: ActivityItem[];
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
  activities: ActivityItem[];
};

class ActivityTab extends React.Component<ActivityTabProps, ActivityTabState> {
  constructor() {
    super();
    this.state = {
      activities: []
    };
  }

  newActivity = (act: Action, itemIDs: string[]) => {
    let item: ActivityItem = {
      action: act,
      action_time: strtime(_.now()),
      entity: itemIDs
    };

    this.setState((state, s) => extend(state, {
      activities: _.concat([item], state.activities)
    }));
  }

  componentDidMount() {
    $.getJSON(this.props.url + '?timestamp=' + _.now(), (j) => {
      this.setState({
        activities: j.activities
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
