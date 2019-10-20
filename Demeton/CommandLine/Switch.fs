[<RequireQualifiedAccess>]
module CommandLine.Switch
    
open CommandLine.Common

let build name = { 
    Name = name
    Description = "" } 

let desc description (arg: CommandSwitch) = 
    { arg with Description = description }

let toPar arg = Switch arg