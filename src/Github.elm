module Github exposing
    ( AuthToken, authToken
    , getBranch, listTags, createBranch
    , getCommit, createCommit
    , PullRequest, getPullRequests, getPullRequest, createPullRequest
    , getFileContents, updateFileContents
    , createFork
    , getComments, createComment
    , CommitSha, Owner, ShaHash, TreeSha, createTree, getBranchZip, getCommitZip, getRepository, getTag, owner, ownerToString, sha, shaToString, updateBranch
    )

{-|

@docs AuthToken, authToken
@docs getBranch, listTags, createBranch
@docs getCommit, createCommit
@docs PullRequest, getPullRequests, getPullRequest, createPullRequest
@docs getFileContents, updateFileContents
@docs createFork


## Issues

@docs getComments, createComment

-}

import Base64
import Bytes exposing (Bytes)
import Bytes.Decode
import Http
import Iso8601
import Json.Decode
import Json.Encode
import Task exposing (Task)
import Time


{-| See <https://docs.github.com/en/rest/reference/repos#get-a-repository>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getRepository :
    { authToken : AuthToken
    , owner : Owner
    , repo : String
    }
    -> Task Http.Error { defaultBranch : String }
getRepository params =
    let
        decoder =
            Json.Decode.map (\defaultBranch -> { defaultBranch = defaultBranch })
                (Json.Decode.field "default_branch" Json.Decode.string)
    in
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ ownerToString params.owner ++ "/" ++ params.repo
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


getBranchZip : { authToken : AuthToken, owner : Owner, repo : String, branchName : String } -> Task Http.Error Bytes
getBranchZip params =
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://github.com/" ++ ownerToString params.owner ++ "/" ++ params.repo ++ "/archive/refs/heads/" ++ params.branchName ++ ".zip"
        , body = Http.emptyBody
        , resolver = bytesResolver
        , timeout = Nothing
        }


getCommitZip : { authToken : AuthToken, owner : Owner, repo : String, sha : ShaHash CommitSha } -> Task Http.Error Bytes
getCommitZip params =
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://github.com/" ++ ownerToString params.owner ++ "/" ++ params.repo ++ "/archive/" ++ shaToString params.sha ++ ".zip"
        , body = Http.emptyBody
        , resolver = bytesResolver
        , timeout = Nothing
        }


{-| See <https://docs.github.com/en/rest/reference/git#get-a-commit>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getCommit :
    { authToken : AuthToken
    , owner : Owner
    , repo : String
    , sha : ShaHash CommitSha
    }
    -> Task Http.Error (ShaHash TreeSha)
getCommit params =
    let
        decoder =
            Json.Decode.at [ "tree", "sha" ] decodeSha
    in
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ ownerToString params.owner ++ "/" ++ params.repo ++ "/git/commits/" ++ shaToString params.sha
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://docs.github.com/en/rest/reference/git#create-a-commit>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createCommit :
    { authToken : AuthToken
    , owner : Owner
    , repo : String
    , message : String
    , tree : ShaHash TreeSha
    , parents : List (ShaHash CommitSha)
    }
    -> Task Http.Error (ShaHash CommitSha)
createCommit params =
    let
        decoder =
            Json.Decode.field "sha" Json.Decode.string
                |> Json.Decode.map sha
    in
    Http.task
        { method = "POST"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ ownerToString params.owner ++ "/" ++ params.repo ++ "/git/commits"
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "message", Json.Encode.string params.message )
                    , ( "tree", encodeSha params.tree )
                    , ( "parents", Json.Encode.list encodeSha params.parents )
                    ]
                )
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://docs.github.com/en/rest/reference/git#get-a-reference>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getBranch :
    { authToken : AuthToken
    , owner : Owner
    , repo : String
    , branchName : String
    }
    -> Task Http.Error (ShaHash CommitSha)
getBranch params =
    let
        decoder =
            Json.Decode.at [ "object", "sha" ] decodeSha
    in
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ ownerToString params.owner ++ "/" ++ params.repo ++ "/git/refs/heads/" ++ params.branchName
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://docs.github.com/en/rest/reference/git#update-a-reference>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
updateBranch :
    { authToken : AuthToken
    , owner : Owner
    , repo : String
    , branchName : String
    , sha : ShaHash CommitSha
    , force : Bool
    }
    -> Task Http.Error (ShaHash CommitSha)
updateBranch params =
    Http.task
        { method = "PATCH"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ ownerToString params.owner ++ "/" ++ params.repo ++ "/git/refs/heads/" ++ params.branchName
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "sha", Json.Encode.string (shaToString params.sha) ), ( "force", Json.Encode.bool params.force ) ]
                )
        , resolver = jsonResolver referenceDecoder
        , timeout = Nothing
        }


{-| See <https://docs.github.com/en/rest/reference/git#get-a-reference>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getTag :
    { authToken : AuthToken
    , owner : Owner
    , repo : String
    , tagName : String
    }
    -> Task Http.Error (ShaHash CommitSha)
getTag params =
    let
        decoder =
            Json.Decode.at [ "object", "sha" ] decodeSha
    in
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ ownerToString params.owner ++ "/" ++ params.repo ++ "/git/refs/tags/" ++ params.tagName
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }



--{-| See <https://docs.github.com/en/rest/reference/git#get-a-tree>
--
--NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.
--
---}
--getTree :
--    { authToken : AuthToken
--    , owner : Owner
--    , repo : String
--    , treeSha : ShaHash
--    }
--    -> Task Http.Error (List TreeNode)
--getTree params =
--    let
--        decoder =
--            Json.Decode.field "tree" (Json.Decode.list decodeTreeNode)
--    in
--    Http.task
--        { method = "GET"
--        , headers = [ authorizationHeader params.authToken ]
--        , url = "https://api.github.com/repos/" ++ ownerToString params.owner ++ "/" ++ params.repo ++ "/git/trees/" ++ shaToString params.treeSha
--        , body = Http.emptyBody
--        , resolver = jsonResolver decoder
--        , timeout = Nothing
--        }


{-| See <https://docs.github.com/en/rest/reference/git#create-a-tree>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createTree :
    { authToken : AuthToken
    , owner : Owner
    , repo : String
    , treeNodes : List { path : String, content : String }
    , baseTree : Maybe (ShaHash TreeSha)
    }
    -> Task Http.Error { treeSha : ShaHash TreeSha }
createTree params =
    let
        decoder =
            Json.Decode.field "sha" decodeSha
                |> Json.Decode.map (\treeSha -> { treeSha = treeSha })
    in
    Http.task
        { method = "POST"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ ownerToString params.owner ++ "/" ++ params.repo ++ "/git/trees"
        , body =
            ( "tree", Json.Encode.list encodeTreeNode params.treeNodes )
                :: (case params.baseTree of
                        Just baseTree ->
                            [ ( "base_tree", encodeSha baseTree ) ]

                        Nothing ->
                            []
                   )
                |> Json.Encode.object
                |> Http.jsonBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }



--
--type TreeNode
--    = Subdirectory { path : String, sha : ShaHash }
--    | Blob { path : String, sha : ShaHash, size : Int }
--    | ExecutableBlob { path : String, sha : ShaHash, size : Int }
--    | Submodule
--    | Symlink


encodeTreeNode : { path : String, content : String } -> Json.Encode.Value
encodeTreeNode treeNode =
    ( "path", Json.Encode.string treeNode.path )
        :: ( "mode", Json.Encode.string "100644" )
        :: ( "type", Json.Encode.string "blob" )
        :: ( "content", Json.Encode.string treeNode.content )
        :: []
        |> Json.Encode.object



--
--decodeTreeNode : Json.Decode.Decoder TreeNode
--decodeTreeNode =
--    Json.Decode.field "mode" Json.Decode.string
--        |> Json.Decode.andThen
--            (\mode ->
--                case String.toInt mode of
--                    Just 100644 ->
--                        Json.Decode.map3 (\path sha_ size -> Blob { path = path, sha = sha_, size = size })
--                            (Json.Decode.field "path" Json.Decode.string)
--                            (Json.Decode.field "sha" decodeSha)
--                            (Json.Decode.field "size" Json.Decode.int)
--
--                    Just 100755 ->
--                        Json.Decode.map3 (\path sha_ size -> ExecutableBlob { path = path, sha = sha_, size = size })
--                            (Json.Decode.field "path" Json.Decode.string)
--                            (Json.Decode.field "sha" decodeSha)
--                            (Json.Decode.field "size" Json.Decode.int)
--
--                    Just 40000 ->
--                        Json.Decode.map2 (\path sha_ -> Subdirectory { path = path, sha = sha_ })
--                            (Json.Decode.field "path" Json.Decode.string)
--                            (Json.Decode.field "sha" decodeSha)
--
--                    Just 160000 ->
--                        Json.Decode.succeed Submodule
--
--                    Just 120000 ->
--                        Json.Decode.succeed Symlink
--
--                    _ ->
--                        Json.Decode.fail ("Invalid mode: " ++ mode)
--            )


referenceDecoder =
    Json.Decode.at [ "object", "sha" ] Json.Decode.string
        |> Json.Decode.map sha


type alias Tag =
    { name : String
    , commitSha : ShaHash CommitSha
    , nodeId : String
    }


decodeTag : Json.Decode.Decoder Tag
decodeTag =
    Json.Decode.map3 Tag
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.at [ "commit", "sha" ] decodeSha)
        (Json.Decode.field "node_id" Json.Decode.string)


{-| See <https://docs.github.com/en/rest/reference/repos#list-repository-tags>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
listTags :
    { authToken : AuthToken
    , owner : Owner
    , repo : String
    }
    -> Task Http.Error (List Tag)
listTags params =
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ ownerToString params.owner ++ "/" ++ params.repo ++ "/tags"
        , body = Http.emptyBody
        , resolver = jsonResolver (Json.Decode.list decodeTag)
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/git/refs/#create-a-reference>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createBranch :
    { authToken : AuthToken
    , repo : String
    , branchName : String
    , sha : ShaHash CommitSha
    }
    -> Task Http.Error ()
createBranch params =
    let
        decoder =
            Json.Decode.succeed ()
    in
    Http.task
        { method = "POST"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/git/refs"
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "ref", Json.Encode.string ("refs/heads/" ++ params.branchName) )
                    , ( "sha", Json.Encode.string (shaToString params.sha) )
                    ]
                )
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| The data returned by [`getPullRequests`](#getPullRequests).
-}
type alias PullRequest =
    { number : Int
    , title : String
    }


decodePullRequest =
    Json.Decode.map2
        PullRequest
        (Json.Decode.at [ "number" ] Json.Decode.int)
        (Json.Decode.at [ "title" ] Json.Decode.string)


{-| See <https://developer.github.com/v3/pulls/#list-pull-requests>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getPullRequests :
    { authToken : AuthToken
    , repo : String
    }
    -> Task Http.Error (List PullRequest)
getPullRequests params =
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/pulls"
        , body = Http.emptyBody
        , resolver = jsonResolver (Json.Decode.list decodePullRequest)
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/pulls/#get-a-single-pull-request>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getPullRequest :
    { authToken : AuthToken
    , repo : String
    , number : Int
    }
    ->
        Task
            Http.Error
            { head :
                { ref : String
                , sha : ShaHash CommitSha
                }
            }
getPullRequest params =
    let
        decoder =
            Json.Decode.map2
                (\headRef headSha ->
                    { head =
                        { ref = headRef
                        , sha = sha headSha
                        }
                    }
                )
                (Json.Decode.at [ "head", "ref" ] Json.Decode.string)
                (Json.Decode.at [ "head", "sha" ] Json.Decode.string)
    in
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/pulls/" ++ String.fromInt params.number
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/pulls/#create-a-pull-request>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createPullRequest :
    { authToken : AuthToken
    , destinationOwner : Owner
    , destinationRepo : String
    , destinationBranch : String
    , sourceBranchOwner : Owner
    , sourceBranch : String
    , title : String
    , description : String
    }
    -> Task Http.Error { url : String }
createPullRequest params =
    let
        decoder =
            Json.Decode.field "url" Json.Decode.string
                |> Json.Decode.map (\url -> { url = url })
    in
    Http.task
        { method = "POST"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ ownerToString params.destinationOwner ++ "/" ++ params.destinationRepo ++ "/pulls"
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "title", Json.Encode.string params.title )
                    , ( "base", Json.Encode.string params.destinationBranch )
                    , ( "head"
                      , Json.Encode.string
                            (if params.destinationOwner == params.sourceBranchOwner then
                                params.sourceBranch

                             else
                                ownerToString params.sourceBranchOwner ++ ":" ++ params.sourceBranch
                            )
                      )
                    , ( "body", Json.Encode.string params.description )
                    ]
                )
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/repos/contents/#get-contents>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getFileContents :
    { authToken : AuthToken
    , repo : String
    , ref : String
    , path : String
    }
    ->
        Task
            Http.Error
            { encoding : String
            , content : String
            , sha : ShaHash a
            }
getFileContents params =
    let
        decoder =
            Json.Decode.map3
                (\encoding content sha_ ->
                    { encoding = encoding
                    , content = content
                    , sha = sha sha_
                    }
                )
                (Json.Decode.at [ "encoding" ] Json.Decode.string)
                (Json.Decode.at [ "content" ] Json.Decode.string)
                (Json.Decode.at [ "sha" ] Json.Decode.string)
    in
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/contents/" ++ params.path ++ "?ref=" ++ params.ref
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/repos/contents/#update-a-file>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
updateFileContents :
    { authToken : AuthToken
    , repo : String
    , branch : String
    , path : String
    , sha : ShaHash a
    , message : String
    , content : String
    }
    ->
        Task
            Http.Error
            { content :
                { sha : ShaHash a
                }
            }
updateFileContents params =
    let
        decoder =
            Json.Decode.map
                (\contentSha ->
                    { content = { sha = contentSha } }
                )
                (Json.Decode.at [ "content", "sha" ] decodeSha)
    in
    Http.task
        { method = "PUT"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/contents/" ++ params.path
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "message", Json.Encode.string params.message )
                    , ( "content", Json.Encode.string (Base64.encode params.content) )
                    , ( "sha", encodeSha params.sha )
                    , ( "branch", Json.Encode.string params.branch )
                    ]
                )
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/issues/comments/#list-comments-on-an-issue>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getComments :
    { authToken : AuthToken
    , repo : String
    , issueNumber : Int
    }
    ->
        Task
            Http.Error
            (List
                { body : String
                , user :
                    { login : String
                    , avatarUrl : String
                    }
                , createdAt : Time.Posix
                , updatedAt : Time.Posix
                }
            )
getComments params =
    let
        decoder =
            Json.Decode.map5
                (\body userLogin userAvatarUrl createdAt updatedAt ->
                    { body = body
                    , user =
                        { login = userLogin
                        , avatarUrl = userAvatarUrl
                        }
                    , createdAt = createdAt
                    , updatedAt = updatedAt
                    }
                )
                (Json.Decode.at [ "body" ] Json.Decode.string)
                (Json.Decode.at [ "user", "login" ] Json.Decode.string)
                (Json.Decode.at [ "user", "avatar_url" ] Json.Decode.string)
                (Json.Decode.at [ "created_at" ] Iso8601.decoder)
                (Json.Decode.at [ "updated_at" ] Iso8601.decoder)
    in
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/issues/" ++ String.fromInt params.issueNumber ++ "/comments"
        , body = Http.emptyBody
        , resolver = jsonResolver (Json.Decode.list decoder)
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/issues/comments/#create-a-comment>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createComment :
    { authToken : AuthToken
    , repo : String
    , issueNumber : Int
    , body : String
    }
    ->
        Task
            Http.Error
            { body : String
            , user :
                { login : String
                , avatarUrl : String
                }
            , createdAt : Time.Posix
            , updatedAt : Time.Posix
            }
createComment params =
    let
        decoder =
            Json.Decode.map5
                (\body userLogin userAvatarUrl createdAt updatedAt ->
                    { body = body
                    , user =
                        { login = userLogin
                        , avatarUrl = userAvatarUrl
                        }
                    , createdAt = createdAt
                    , updatedAt = updatedAt
                    }
                )
                (Json.Decode.at [ "body" ] Json.Decode.string)
                (Json.Decode.at [ "user", "login" ] Json.Decode.string)
                (Json.Decode.at [ "user", "avatar_url" ] Json.Decode.string)
                (Json.Decode.at [ "created_at" ] Iso8601.decoder)
                (Json.Decode.at [ "updated_at" ] Iso8601.decoder)
    in
    Http.task
        { method = "POST"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/issues/" ++ String.fromInt params.issueNumber ++ "/comments"
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "body", Json.Encode.string params.body )
                    ]
                )
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


type Owner
    = Owner String


owner : String -> Owner
owner =
    Owner


ownerToString : Owner -> String
ownerToString (Owner owner_) =
    owner_


type AuthToken
    = AuthToken String


authToken : String -> AuthToken
authToken =
    AuthToken


type TreeSha
    = TreeSha Never


type CommitSha
    = CommitSha Never


type ShaHash a
    = ShaHash String


sha : String -> ShaHash a
sha =
    ShaHash


shaToString : ShaHash a -> String
shaToString (ShaHash shaHash) =
    shaHash


decodeSha : Json.Decode.Decoder (ShaHash a)
decodeSha =
    Json.Decode.string |> Json.Decode.map sha


encodeSha : ShaHash a -> Json.Encode.Value
encodeSha =
    shaToString >> Json.Encode.string


{-| See <https://docs.github.com/en/rest/reference/repos#create-a-fork>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createFork :
    { authToken : AuthToken
    , owner : Owner
    , repo : String
    }
    ->
        Task
            Http.Error
            { owner : Owner
            , repo : String
            }
createFork params =
    let
        decoder =
            Json.Decode.map2
                (\owner_ repo ->
                    { owner = owner owner_
                    , repo = repo
                    }
                )
                (Json.Decode.at [ "owner", "login" ] Json.Decode.string)
                (Json.Decode.at [ "name" ] Json.Decode.string)
    in
    Http.task
        { method = "POST"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ ownerToString params.owner ++ "/" ++ params.repo ++ "/forks"
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


authorizationHeader : AuthToken -> Http.Header
authorizationHeader (AuthToken authToken_) =
    Http.header "Authorization" ("token " ++ authToken_)


jsonResolver : Json.Decode.Decoder a -> Http.Resolver Http.Error a
jsonResolver decoder =
    Http.stringResolver <|
        \response ->
            case response of
                Http.GoodStatus_ _ body ->
                    Json.Decode.decodeString decoder body
                        |> Result.mapError Json.Decode.errorToString
                        |> Result.mapError Http.BadBody

                Http.BadUrl_ message ->
                    Err (Http.BadUrl message)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)


bytesResolver : Http.Resolver Http.Error Bytes
bytesResolver =
    Http.bytesResolver <|
        \response ->
            case response of
                Http.GoodStatus_ _ body ->
                    Ok body

                Http.BadUrl_ message ->
                    Err (Http.BadUrl message)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)
