module App

open System
open Elmish
open Elmish.React
open Fable.React 
open Fable.React.Props

[<RequireQualifiedAccess>]
type TodoFilter =
  | All
  | CompletedOnly
  | NotCompletedYet

type Todo = {
  Id : int
  Description : string
  Completed : bool
}

type TodoBeingEdited = {
  Id: int
  Description: string
} 

type State = { 
  TodoList: Todo list 
  NewTodo : string 
  Filter : TodoFilter
  TodosBeingEdited : TodoBeingEdited list
}

type Msg =
  | SetNewTodo of string 
  | AddNewTodo 
  | DeleteTodo of int
  | ToggleCompleted of int
  | CancelEdit of int
  | ApplyEdit of int
  | StartEditingTodo of int 
  | SetEditedDescription of int * string 
  | SetFilter of TodoFilter

  
let init() = { 
  NewTodo = ""
  TodosBeingEdited = [ ]
  Filter = TodoFilter.All
  TodoList = [ 
    { Id = 1; Description = "Learn F#"; Completed = false } 
    { Id = 2; Description = "Learn Elmish"; Completed = true } 
  ]
}

let update (msg: Msg) (state: State) =
  match msg with
  | SetNewTodo desc -> 
      { state with NewTodo = desc }
  
  | AddNewTodo when String.IsNullOrWhiteSpace state.NewTodo ->
      state 

  | AddNewTodo ->
      let nextTodoId = 
        match state.TodoList with
        | [ ] -> 1
        | elems -> 
            elems
            |> List.maxBy (fun todo -> todo.Id)  
            |> fun todo -> todo.Id + 1

      let nextTodo = 
        { Id = nextTodoId
          Description = state.NewTodo
          Completed = false }
          
      { state with 
          NewTodo = ""
          TodoList = List.append state.TodoList [nextTodo] }

  | DeleteTodo todoId ->
      let nextTodoList = 
        state.TodoList
        |> List.filter (fun todo -> todo.Id <> todoId)
      
      { state with TodoList = nextTodoList }

  | ToggleCompleted todoId ->
      let nextTodoList = 
        state.TodoList
        |> List.map (fun todo -> 
           if todo.Id = todoId 
           then { todo with Completed = not todo.Completed }
           else todo)
 
      { state with TodoList = nextTodoList }

  | StartEditingTodo todoId -> 
      let nextTodosBeingEdited = 
        if List.exists (fun todo -> todo.Id = todoId) state.TodosBeingEdited
        then state.TodosBeingEdited
        else
          state.TodoList
          |> List.tryFind (fun todo -> todo.Id = todoId) 
          |> Option.map (fun todo -> { Id = todoId; Description = todo.Description })
          |> Option.map List.singleton
          |> Option.defaultValue [ ]
          |> List.append state.TodosBeingEdited
      
      { state with TodosBeingEdited = nextTodosBeingEdited } 

  | CancelEdit todoId -> 
      let nextTodosBeingEdited = 
        state.TodosBeingEdited
        |> List.filter (fun todo -> todo.Id <> todoId)
      
      { state with TodosBeingEdited = nextTodosBeingEdited }
  
  | ApplyEdit todoId -> 
      let modifiedTodoList = 
        state.TodosBeingEdited
        |> List.tryFind (fun todo -> todo.Id = todoId)
        |> function 
           | None -> state.TodoList 
           | Some todoBeingEdited when todoBeingEdited.Description = "" -> state.TodoList 
           | Some todoBeingEdited ->
              state.TodoList
              |> List.map (fun todo -> 
                  if todo.Id = todoBeingEdited.Id
                  then { todo with Description = todoBeingEdited.Description }
                  else todo)
      
      let modifiedTodosBeingEdited = 
        state.TodosBeingEdited
        |> List.filter (fun todo -> todo.Id <> todoId)
          
      { state with TodoList = modifiedTodoList; TodosBeingEdited = modifiedTodosBeingEdited }

  | SetEditedDescription (id, newText) -> 
      let modifiedTodosBeingEdited = 
        state.TodosBeingEdited
        |> List.map (fun todoBeingEdited -> 
          if todoBeingEdited.Id = id 
          then { todoBeingEdited with Description = newText }
          else todoBeingEdited) 
      
      { state with TodosBeingEdited = modifiedTodosBeingEdited }

  | SetFilter filter ->
      {  state with Filter = filter }


let createTodoTextbox state dispatch = 
  div [ Class "field has-addons" ] [
    div [ Class "control is-expanded" ] [ 
      input [ 
        Class "input is-medium"
        valueOrDefault state.NewTodo
        OnChange (fun ev -> dispatch (SetNewTodo ev.Value)) 
      ]
    ] 
    div [ Class "control" ] [ 
      button [ Class "button is-primary is-medium"; OnClick (fun _ -> dispatch AddNewTodo) ] [ 
        i [ Class "fa fa-plus" ] [ ]
      ]
    ] 
  ] 

let renderEditForm (todoBeingEdited: TodoBeingEdited) (todo: Todo) (dispatch: Msg -> unit) = 
  let saveButtonClass = 
    classList [
      "button", true 
      "is-primary", todoBeingEdited.Description <> todo.Description
      "is-outlined", todoBeingEdited.Description = todo.Description
    ] 
  
  div [ Class "box" ] [
    div [ Class "field is-grouped" ] [ 
      div [ Class "control is-expanded" ] [
        input [ 
          Class "input is-medium"; 
          valueOrDefault todoBeingEdited.Description; 
          OnChange (fun ev -> dispatch (SetEditedDescription (todoBeingEdited.Id, ev.Value))) 
        ]
      ]
      div [ Class "control buttons" ] [
        button [ saveButtonClass; OnClick (fun _ -> dispatch (ApplyEdit (todoBeingEdited.Id)))  ] [
          i [ Class "fa fa-save" ] [ ] 
        ] 
        button [ Class "button is-warning"; OnClick (fun _ -> dispatch (CancelEdit (todoBeingEdited.Id))) ] [ 
          i [ Class "fa fa-arrow-right" ] [ ] 
        ] 
      ]
    ]
  ]

let renderTodo (todo: Todo) (dispatch: Msg -> unit) = 
  let checkButtonStyle = 
    classList [ 
      "button", true
      "is-success", todo.Completed
      "is-outlined", not todo.Completed 
    ]
    
  div [ Class "box" ] [ 
    div [ Class "columns is-mobile" ] [ 
      div [ Class "column" ] [
        p [ Class "subtitle" ] [ str todo.Description ] 
      ]
      div [ Class "column is-5" ] [
        div [ Class "buttons is-right" ] [
          button [ checkButtonStyle; OnClick(fun _ -> dispatch (ToggleCompleted todo.Id))  ] [
            i [ Class "fa fa-check" ] [ ] 
          ] 
          button [ Class "button is-primary"; OnClick (fun _ -> dispatch (StartEditingTodo todo.Id))  ] [
            i [ Class "fa fa-edit" ] [ ] 
          ] 
          button [ Class "button is-danger"; OnClick (fun _ -> dispatch (DeleteTodo todo.Id)) ] [ 
            i [ Class "fa fa-times" ] [ ] 
          ] 
        ]
      ]
    ]
  ]  

let renderFilterTabs (usedFilter: TodoFilter) (dispatch: Msg -> unit) = 
  let setFilter filter = OnClick (fun _ -> dispatch (SetFilter filter))
  
  let tab filter name = 
    li [ classList [ "is-active", usedFilter = filter ]; setFilter filter ] [
      a [ ] [ str name ]
    ]

  div [ Class "tabs is-toggle is-fullwidth" ] [ 
    ul [ ] [
      tab TodoFilter.All "All"
      tab TodoFilter.CompletedOnly "Completed"
      tab TodoFilter.NotCompletedYet "Not completed"
    ]
  ] 

let filterTodos (filter: TodoFilter) (todos: Todo list) = 
  match filter with 
  | TodoFilter.All -> todos
  | TodoFilter.CompletedOnly -> List.filter (fun todo -> todo.Completed) todos
  | TodoFilter.NotCompletedYet -> List.filter (fun todo -> not todo.Completed) todos
      
let render (state: State) (dispatch: Msg -> unit) =
  let filteredTodos = filterTodos state.Filter state.TodoList
  let beingEdited (item: Todo) = 
    state.TodosBeingEdited
    |> List.tryFind (fun todo -> todo.Id = item.Id)

  div [ Style [ Padding 20 ] ] [
    h3 [ Class "title" ] [ str "Elmish To-Do list" ]
    createTodoTextbox state dispatch
    renderFilterTabs state.Filter dispatch
    div [ Class "content" ] [ 
      for todo in filteredTodos -> 
        match beingEdited todo with 
        | Some todoBeingEdited -> 
            renderEditForm todoBeingEdited todo dispatch
        | None -> 
            renderTodo todo dispatch
    ]
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run