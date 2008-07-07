module tests/test

open util/integer

sig LongString, Email, Date, DateTime { }  // Also, we also use "sig Int" a lot

enum String { StringAlex, StringBrian, StringEmina, StringDaniel, StringFelix, StringIan, StringJukka,
              StringMIT, StringNokia, StringWindowsDiscussion, StringLinuxDiscussion,
              StringMachineLearning, StringNumericOptimization, StringTrueMultithreading, StringEfficientCompilerConstruction,
              StringHowToOptimize, StringHelpNeeded }

enum Boolean { True, False }

abstract sig Entity {
  owners: set Entity,    // has no special meaning, other than it is associated at time of creation, and can be reassigned later; order is preserved
  created: DateTime,     // automatically assigned at creating time
  lastModified: DateTime // automatically updated whenever any field is modified
}

one sig me in Entity { }

abstract sig LoginUser extends Entity { suspended: Boolean,  email: disjoint Email,  password: String } // magic GUI support

one sig ADD1, ADD2, DELETE1, DELETE2 { }
enum Action { R, A, D, AS, DS }
fun W: set Action  { A+D }
fun RW: set Action { R+W }
let ADD = ADD1->ADD2
let DELETE = DELETE1->DELETE2

// helper method that returns the set of entities controlled by the current user
fun own: set Entity { Entity } // me.super=True => Entity else (owners.me + {g:tentatives.me | no g.members+g.admins} + {g:members.me | no g.admins} + admins.me) }

// this specially named function is used to determine the valid read/add/delete permissions
fun policy : ADD->sig$ + DELETE->Entity + Action->Entity->field$
{
 // everyone can read their own User.email and User.getMail
 R -> me -> (LoginUser$email + User$getMail)

 // super users can modify everything
 + (A+D) -> (me.super=True => Entity else none) -> field$

 // owner can read/write everything except Entity.{owner/created/lastModified}, User.super, User.suspended, and Topic.{replies/pinned/readOnly}
 + (R+A+D) -> own -> (field$ - Entity$owners - Entity$created - Entity$lastModified - User$super - LoginUser$suspended - Topic$replies - Topic$pinned - Topic$readOnly)

 // You can remove yourself from a group's "member list"
 + DS -> Group -> Group$members

 // super users can create anything
 + ADD -> (me.super=True => (User$ + Group$ + Talk$ + Forum$ + Topic$ + Msg$) else (Group$ + Talk$ + Topic$ + Msg$))

 // you can delete yourself, any group you manage, and any talk you own; super users can delete everything
 + DELETE -> own
}

---------------------------------------------------------------------------------------------------------------------------------------------------------

sig User extends LoginUser {
    name    : disjoint String,  /** fact="Name cannot be empty, or be the same as an existing user." */
    bio     : lone LongString,
    super   : Boolean,          /** label="Administrator" */
    getMail : Boolean           /** label="Receives forum posts as email" */
}

fun User.groups : set Group { members.this + (Group<:owners).this }
fun User.points : Int       { 25.mul[#(Talk<:owners . this)] + 20.mul[#(Msg<:owners . this)]  }

---------------------------------------------------------------------------------------------------------------------------------------------------------

sig Group extends Entity {
    name       : disjoint String,                  /** fact="Group name cannot be empty, or have the same name as an existing group." */
    descr      : lone LongString,                  /** label="Description" */
    homeMsg    : lone LongString,                  /** label="Welcome Message" */
    restricted : Boolean,                          /** label="Group membership is restricted" */
    members    : set Member                        /** default=hide, own, expand */
}

sig Member { type: Type }

enum Type { Admin, Regular, Tentative }

pred members.onAdd {
  let g=this.univ, u=univ.this.owners, t=univ.this.type {
     // you cannot have different membership type simultaneously
     u  !in g.members.owners
     // if you're not a group owner, then membership type must be "Regular" (or "Tentative" if group is restricted)
     me !in g.owners => (g.restricted=False => t=Regular else t=Tentative)
  }
}

sig Talk extends Entity {
    title    : String       /** fact="The title of a talk cannot be empty." */,
    datetime : DateTime,
    location : lone LongString,
    descr    : lone LongString,
    speakers : set String,
    tags     : set Group
} {
    some speakers         /** fact="The list of speakers cannot be empty." */
    tags in owners.groups /** fact="You cannot tag a talk with a group that you don't belong to." */
}

sig Forum extends Entity {      // view sorted by weight then name
    name     : disjoint String  /** fact="The forum name cannot be empty, or be the same as an existing forum." */,
    descr    : lone LongString,
    weight   : Int,
    readOnly : Boolean,
    topics   : set Topic        /** default=hide, own, expand */
}

fun Forum.numTopics : Int { #(this.topics) }
fun Forum.numText   : Int { #(this.topics) + #(this.topics.replies) }

sig Topic extends Entity { // view sorted by pinned then by last post time
    title    : String      /** fact="The subject cannot be empty" */,
    text     : LongString  /** fact="The message cannot be empty" */,
    pinned   : Boolean,
    readOnly : Boolean,
    replies  : set Msg     /** default=hide, own, expand */
}

fun Topic.parent      : Forum    { topics.this     }
fun Topic.numReplies  : Int      { #(this.replies) }
fun Topic.firstAuthor : User     { this.owners     }
fun Topic.firstDate   : DateTime { this.created    }

pred Topic.onAdd {
  // only super users can create a pinned topic, or create a readonly topic, or post in a readonly forum
  me.super = True || this.pinned + this.readOnly + this.parent.readOnly = False
}

sig Msg extends Entity {  // view sorted by created time
    text : LongString     /** fact="The message cannot be empty" */
}

fun Msg.parent : Topic { replies.this }

pred Msg.onAdd {
  me.super = True || this.parent.readOnly = False
}

run {
let namez = StringAlex + StringBrian + StringEmina + StringDaniel + StringFelix + StringIan + StringJukka |
let groupz = StringMIT + StringNokia |
let forumz = StringWindowsDiscussion + StringLinuxDiscussion |
let talkz = StringMachineLearning + StringNumericOptimization + StringTrueMultithreading + StringEfficientCompilerConstruction |
let topicz = StringHowToOptimize + StringHelpNeeded |
{
(!lone User)
(all x:Entity | some x.owners)
(all x:Entity | x.owners in User)
(all x:User | some x.bio && x.name in namez && some x.groups)
(all x:Group | some x.descr && some x.homeMsg && some x.requests && some x.members && some x.owners && x.name in groupz)
(all x:Talk | some x.location)
(all x:Talk | some x.descr)
(all x:Talk | some x.tags)
(all x:Talk | x.title in talkz)
(all x:Forum | some x.descr && x.name in forumz)
(all x:Topic | x.title in topicz)
(all disj x, y: Topic | x.title != y.title)
(all disj x, y: Talk | x.title != y.title)
(all disj x, y: Msg | x.text != y.text)
}
} for 7 but 1 DateTime, exactly 7 User, exactly 2 Group, exactly 4 Talk, exactly 2 Forum, exactly 2 Topic, exactly 4 Msg expect 1

