'use strict';
var json, hjson;
var hcom = [];
var hcom_index = -1;

function mparser(str) {
  let args = _.split(str, ' ');
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
      tags: "",
      due: "",
      message: ""
    }
  });
}

function strtime(dtime) {
  if (dtime == "") {
    return "";
  } else {
    return moment(dtime).format("YYYY-MM-DD HH:mm:ss");
  }
}

function mk_item(name, icon, color, tags, due, message) {
  let now = new Date();

  return {
    "id": now.getTime().toString(),
    "name": name,
    "tags": tags,
    "icon": icon,
    "color": color,
    "created_time": strtime(now),
    "due": strtime(Date.parse(due) || ""),
    "message": message
  }
}

function mk_action(id) {
  return {
    "action": "",
    "action_time": strtime(moment()),
    "entity": [id]
  }
}

function mk_del_action(id) {
  return $.extend(mk_action(id), {
    "action": "delete"
  });
}

function mk_add_action(id) {
  return $.extend(mk_action(id), {
    "action": "add"
  });
}

function mk_edit_action(id) {
  return $.extend(mk_action(id), {
    "action": "edit"
  });
}

function promise_sync() {
  return new Promise((resolve, reject) => {
    $.ajax({
      url: "/update",
      type: 'POST',
      data: JSON.stringify([json, hjson]),
      dataType: 'json',
      contentType: 'application/json'
    })
    .done((d) => {
      $("#modified").hide();
      resolve();
    })
    .fail((d) => { reject(); });
  });
}

(function() {
  let promise_json_load = file => {
    return new Promise((resolve, reject) => {
      $.getJSON(file + '?timestamp=' + _.now(), ([json_, hjson_]) => {
        json = json_;
        hjson = hjson_;
      })
      .done(() => {
        update_feed();
        update_activity();
        resolve();
      });
    });
  };

  let terminal_event = () => {
    let promise_parse_add = (mobj) => new Promise((resolve, reject) => {
      if (mobj._.length >= 2 && mobj._[0] == "add") {
        let tagl = _.split(mobj["tags"], ',')
        let item = mk_item(mobj._[1], mobj["icon"], mobj["color"], tagl, mobj["due"], mobj["message"]);
        json["items"][item["id"]] = item;
        json["undone"].unshift(item["id"]);
        hjson["actions"].unshift(mk_add_action(item["id"]));
        $("#modified").show();
        update_feed();
        update_activity();
        resolve();
      } else {
        reject();
      }
    });

    let promise_parse_save = (mobj) => new Promise((resolve, reject) => {
      if (mobj._.length >= 1 && mobj._[0] == "save") {
        promise_sync()
          .then(() => {
            saving_start();
            resolve();
          })
          .catch(() => { reject(); });
      } else {
        reject();
      }
    });

    let promise_parse_list = (mobj) => new Promise((resolve, reject) => {
      if (mobj._.length >= 1 && mobj._[0] == "list") {
        if (mobj._[1] == "bytag") {
          json["undone"] = _.sortBy(json["undone"], (k) => {
            return json["items"][k]["tags"];
          });
          update_feed();
        } else if (mobj._[1] == "byicon") {
          json["undone"] = _.sortBy(json["undone"], (k) => {
            return json["items"][k]["icon"];
          });
          update_feed();
        } else if (mobj._[1] == "bycolor") {
          json["undone"] = _.sortBy(json["undone"], (k) => {
            return json["items"][k]["color"];
          });
          update_feed();
        } else {
          json["undone"] = _.sortBy(json["undone"], (k) => {
            return json["items"][k]["created_time"];
          }).reverse();
          update_feed();
        }
        resolve();
      } else {
        reject();
      }
    });

    $("#terminal").bind("enterKey", function(e) {
      let text = $(this).val();
      let mobj = mparser(text);
      if (text != "") {
        hcom.unshift(text);
      }

      promise_parse_add(mobj)
        .then(() => { $(this).val(""); })
        .catch(() => {
          promise_parse_save(mobj)
            .then(() => { $(this).val(""); })
            .catch(() => {
              promise_parse_list(mobj)
                .then(() => { $(this).val(""); })
                .catch(() => { return ; });
            });
        });
    });

    $("#terminal").keyup(function(e) {
      if (e.keyCode == 13) { $(this).trigger("enterKey"); }
      if (e.keyCode == 38) {
        if (hcom_index != hcom.length - 1) { hcom_index += 1; }
        if (hcom_index != -1) { $(this).val(hcom[hcom_index]); }
      }
      if (e.keyCode == 40) {
        if (hcom_index != -1) { hcom_index -= 1; }
        if (hcom_index == -1) {
          $(this).val("");
        } else {
          $(this).val(hcom[hcom_index]);
        }
      }
    });
  };

  promise_json_load("todo/20160620195110.json")
    .then(terminal_event);

  let update_timestamp = () => {
    _($(".timestamp")).each((v) => {
      let d = $(v).attr("data-timestamp");
      $(v).text(moment(d).fromNow());
    });
  };

  setInterval(update_timestamp, 1000*60);

  let saving_start = () => {
    $("#saving-state").show();
    $("#saving-state").html(`<i class="circle notched loading icon"></i> Saving`);

    promise_sync()
      .then(() => {
        setTimeout(() => {
          $("#saving-state").html(`<i class="checkmark icon"></i> Saved!`);

          setTimeout(() => {
            $("#saving-state").fadeOut();
          }, 1000);
        }, 500);
      })
      .catch(() => { return; });
  };

  setInterval(saving_start, 1000*30);
  $("#terminal").focus();
})();

let itemDOM_string = `
  <div class="event">
    <div class="label">
      <i class="fitted circular <%= color %> <%= icon %> icon"></i>
    </div>

    <div class="content">
      <div class="ui accordion">
        <div class="title" style="padding: 0">
          <div class="summary">
            <%= name %>

            <div class="date timestamp" data-timestamp="<%= created_time %>">
              <%= moment(created_time).fromNow() %>
            </div>
          </div>
        </div>
        <div class="content">
          <form id="<%= id %>" class="edit-form ui small form" onsubmit="return false;">
            <div class="field">
              <label>TODO:</label>
              <input type="text" name="todo-name" value="<%= name %>">
            </div>

            <div class="field">
              <label>Message:</label>
              <textarea name="item-message"><%= message %></textarea>
            </div>

            <div class="two fields">
              <div class="field">
                <label>Due:</label>
                <input type="text" name="due-date" class="datetimepicker" value="<%= due %>">
              </div>

              <div class="field">
                <label>Created:</label>
                <input type="text" name="created-date" value="<%= created_time %>" disabled>
              </div>
            </div>

            <div class="three fields">
              <div class="field">
                <label>Icon:</label>

                <div class="ui top left pointing dropdown basic tiny fluid button">
                  <span class="text icon-select">
                    <div class="item">
                      <i class="pencil icon"></i> <span class="icon-name">pencil</span>
                    </div>
                  </span>
                  <div class="menu">
                    <div class="ui icon search input" style="width: auto;">
                      <i class="search icon"></i>
                      <input type="text" placeholder="Search tags...">
                    </div>
                    <div class="scrolling menu">
                      <div class="item">
                        <i class="pencil icon"></i> <span class="icon-name">pencil</span>
                      </div>
                      <div class="item">
                        <i class="calendar icon"></i> <span class="icon-name">calendar</span>
                      </div>
                      <div class="item">
                        <i class="checkmark icon"></i> <span class="icon-name">checkmark</span>
                      </div>
                      <div class="item">
                        <i class="tasks icon"></i> <span class="icon-name">tasks</span>
                      </div>
                      <div class="item">
                        <i class="alarm icon"></i> <span class="icon-name">alarm</span>
                      </div>
                    </div>
                  </div>
                </div>
              </div>

              <div class="field">
                <label>Color:</label>

                <div class="ui top left pointing dropdown basic tiny fluid button">
                  <span class="text color-select">
                    <div class="item">
                      <div class="ui red empty circular label"></div> <span class="color-name">red</span>
                    </div>
                  </span>
                  <div class="menu">
                    <div class="ui icon search input" style="width: auto;">
                      <i class="search icon"></i>
                      <input type="text" placeholder="Search tags...">
                    </div>
                    <div class="scrolling menu">
                      <div class="item">
                        <div class="ui red empty circular label"></div> <span class="color-name">red</span>
                      </div>
                      <div class="item">
                        <div class="ui orange empty circular label"></div> <span class="color-name">orange</span>
                      </div>
                      <div class="item">
                        <div class="ui yellow empty circular label"></div> <span class="color-name">yellow</span>
                      </div>
                      <div class="item">
                        <div class="ui olive empty circular label"></div> <span class="color-name">olive</span>
                      </div>
                      <div class="item">
                        <div class="ui green empty circular label"></div> <span class="color-name">green</span>
                      </div>
                      <div class="item">
                        <div class="ui teal empty circular label"></div> <span class="color-name">teal</span>
                      </div>
                      <div class="item">
                        <div class="ui blue empty circular label"></div> <span class="color-name">blue</span>
                      </div>
                      <div class="item">
                        <div class="ui violet empty circular label"></div> <span class="color-name">violet</span>
                      </div>
                      <div class="item">
                        <div class="ui purple empty circular label"></div> <span class="color-name">purple</span>
                      </div>
                      <div class="item">
                        <div class="ui pink empty circular label"></div> <span class="color-name">pink</span>
                      </div>
                      <div class="item">
                        <div class="ui brown empty circular label"></div> <span class="color-name">brown</span>
                      </div>
                      <div class="item">
                        <div class="ui grey empty circular label"></div> <span class="color-name">grey</span>
                      </div>
                      <div class="item">
                        <div class="ui black empty circular label"></div> <span class="color-name">black</span>
                      </div>
                    </div>
                  </div>
                </div>
              </div>

              <div class="field">
                <label>Tags:</label>
                <input type="text" name="item-tags" value="<%= tags %>">
              </div>
            </div>

            <button id="<%= id %>" class="submit ui tiny violet button" type="submit">Submit</button>
            <button id="<%= id %>" class="delete ui tiny button" type="submit">Delete</button>
          </form>
        </div>

        <% if (message.length != 0) { %>
        <div class="extra text">
          <%= message %>
        </div>
        <% } %>
      </div>

      <div class="meta">
        <a class="like">
          <i class="tags icon"></i> <%= _.join(tags, ', ') %>
        </a>
      </div>

      <div id="<%= id %>" class="done-button ui vertical animated right floated compact mini button">
        <div class="visible content">
          <i class="spinner icon"></i> Undone
        </div>
        <div class="hidden content">
          <i class="check icon"></i> Done?
        </div>
      </div>
    </div>
  </div>
  `;

let activityDOM_string = `
  <div class="ui feed">
    <div class="event">
      <div class="label">
        <i class="fitted <%= icon %> <%= color %> icon"></i>
      </div>
      <div class="content">
        <div class="summary">
          <%= message %>

          <div class="date timestamp" data-timestamp="<%= action_time %>">
            <%= moment(action_time).fromNow() %>
          </div>
        </div>
      </div>
    </div>
  </div>`

let feedDOM_string = `
  <% if (overdue_items.length != 0) { %>
  <h4 class="ui horizontal divider header">
    Overdue
  </h4>
  <% } %>

  <%= overdue_items %>

  <% if (today_items.length != 0) { %>
  <h4 class="ui horizontal divider header">
    Today
  </h4>
  <% } %>

  <%= today_items %>

  <% if (thisweek_items.length != 0) { %>
  <h4 class="ui horizontal divider header">
    This Week
  </h4>
  <% } %>

  <%= thisweek_items %>

  <% if (other_items.length != 0) { %>
  <h4 class="ui horizontal divider header">
    Others
  </h4>
  <% } %>

  <%= other_items %>

  <% if (nodue_items.length != 0) { %>
  <h4 class="ui horizontal divider header">
    Todo
  </h4>
  <% } %>

  <%= nodue_items %>

  `;

function update_feed() {
  let itemHTML = item => {
    return _.template(itemDOM_string)(item_obj(item));
  };
  let feedHTML = json => {
    let uditems = _.map(json["undone"], k => json["items"][k]);
    let [nodue, nonodue] = _.partition(uditems, d => { return (!('due' in d) || d['due'] == ""); })
    let [overdue, nooverdue] = _.partition(nonodue, d => { return moment(d['due']) <= moment(); })
    let [inday, noinday] = _.partition(nooverdue, d => { return moment(d['due']) < moment().endOf('day'); })
    let [inweek, longdue] = _.partition(noinday, d => { return moment(d['due']) < moment().add(1, 'week'); })
    return _.template(feedDOM_string)({
      "overdue_items": _.join(_.map(overdue, itemHTML), '\n'),
      "today_items": _.join(_.map(inday, itemHTML), '\n'),
      "thisweek_items": _.join(_.map(inweek, itemHTML), '\n'),
      "other_items": _.join(_.map(longdue, itemHTML), '\n'),
      "nodue_items": _.join(_.map(nodue, itemHTML), '\n')
    });
  };

  $("#todo-items").html(feedHTML(json));
  $('.ui.accordion').accordion();
  $('.ui.dropdown').dropdown();
  $('.datetimepicker').datetimepicker({
    format: 'Y-m-d H:i:s',
    step: 30
  });

  _($(".done-button")).each((v) => {
    $(v).click(() => {
      let id = $(v).attr("id");
      json["done"].unshift(id);
      json["undone"] = _.without(json["undone"], id);
      hjson["actions"].unshift(mk_del_action(id));
      $("#modified").show();
      update_feed();
      update_activity();
    });
  });

  _($(".edit-form")).each((v) => {
    $(v).find('.submit').click(() => {
      let id = $(v).attr("id");
      let name = $(v).find('[name=todo-name]').val();
      let due = $(v).find('[name=due-date]').val();
      let icon = $(v).find('.icon-select').find('.icon-name').text();
      let color = $(v).find('.color-select').find('.color-name').text();
      let message = $(v).find('[name=item-message]').val();
      let tags = $(v).find('[name=item-tags]').val();
      _.each(json["items"], (item) => {
        if (item["id"] == id) {
          item["name"] = name;
          item["due"] = due;
          item["icon"] = icon;
          item["color"] = color;
          item["message"] = message;
          item["tags"] = _.split(tags, ',');
          hjson["actions"].unshift(mk_edit_action(id));
          $("#modified").show();
        }
      });

      update_feed();
      update_activity();
    });

    $(v).find('.delete').click(() => {
      let id = $(v).attr("id");
      delete json["items"][id];
      json["undone"] = _.without(json["undone"], id);
      _.each(hjson["actions"], (actv) => {
        actv["entity"] = _.without(actv["entity"], id);
      });
      _.remove(hjson["actions"], (actv) => { return actv["entity"].length == 0; });

      update_feed();
      update_activity();
    });
  });
}

function item_obj(item) {
  return {
    "name": item["name"],
    "color": ("color" in item && item["color"]) || "teal",
    "icon": item["icon"],
    "created_time": item["created_time"],
    "tags": item["tags"],
    "id": item["id"],
    "due": ("due" in item && item["due"]) || "",
    "message": ("message" in item && item["message"]) || ""
  }
}

function activitiy_obj(actv) {
  let ent0 = json["items"][actv["entity"][0]];
  let actv_obj = {
    "icon": ent0["icon"],
    "color": ("color" in ent0 && ent0["color"]) || "teal",
    "action_time": actv["action_time"]
  };

  if (actv["action"] == "delete") {
    return $.extend(actv_obj, {
      "message": `
        <div class="ui green horizontal label">
          <span><i class="fitted checkmark icon"></i> Done</span>
        </div>\n` + ent0["name"],
    });
  } else if (actv["action"] == "add") {
    return $.extend(actv_obj, {
      "message": `
        <div class="ui red horizontal label">
          <span><i class="fitted add circle icon"></i> Add</span>
        </div>\n` + ent0["name"],
    });
  } else if (actv["action"] == "edit") {
    return $.extend(actv_obj, {
      "message": `
        <div class="ui violet horizontal label">
          <span><i class="fitted edit icon"></i> Edit</span>
        </div>\n` + ent0["name"],
    });
  } else {
    console.error("Invalid action: " + actv["action"]);
  }
}

function update_activity() {
  let activityHTML = actv => {
    return _.template(activityDOM_string)(activitiy_obj(actv));
  };
  let feedHTML = json => {
    let [nodue, nonodue] = _.partition(json["actions"], d => { return (!('due' in d) || d['due'] == ""); })
    let [overdue, nooverdue] = _.partition(nonodue, d => { return moment(d['due']) <= moment(); })
    let [inday, noinday] = _.partition(nooverdue, d => { return moment(d['due']) < moment().endOf('day'); })
    let [inweek, longdue] = _.partition(noinday, d => { return moment(d['due']) < moment().add(1, 'week'); })
    return _.template(feedDOM_string)({
      "overdue_items": _.join(_.map(overdue, activityHTML), '\n'),
      "today_items": _.join(_.map(inday, activityHTML), '\n'),
      "thisweek_items": _.join(_.map(inweek, activityHTML), '\n'),
      "other_items": _.join(_.map(longdue, activityHTML), '\n'),
      "nodue_items": _.join(_.map(nodue, activityHTML), '\n')
    });
  };

  $("#history-items").html(feedHTML(hjson));
}
