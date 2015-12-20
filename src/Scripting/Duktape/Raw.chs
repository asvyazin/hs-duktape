{-# LANGUAGE ForeignFunctionInterface #-}
module Scripting.Duktape.Raw where


import Control.Monad
import Data.Bits
import Data.List
import Foreign hiding (void)
import Foreign.C.String
import Foreign.C.Types


#include "duktape.h"


{#pointer *duk_context as CDukContext foreign finalizer duk_destroy_heap newtype#}


{#typedef duk_size_t CSize#}
{#typedef duk_errcode_t Int#}
{#typedef duk_bool_t Bool#}
{#typedef duk_idx_t Int#}


type CDukAllocFunction =
  Ptr () -> CSize -> IO (Ptr ())


type CDukReallocFunction =
  Ptr () -> Ptr () -> CSize -> IO (Ptr ())


type CDukFreeFunction =
  Ptr () -> Ptr () -> IO ()


type CDukFatalFunction =
  Ptr CDukContext -> Int -> CString -> IO ()


{#fun duk_create_heap as ^ { id `FunPtr CDukAllocFunction'
                           , id `FunPtr CDukReallocFunction'
                           , id `FunPtr CDukFreeFunction'
                           , `Ptr ()'
                           , id `FunPtr CDukFatalFunction' } -> `CDukContext'#}


dukCreateHeapDefault :: IO CDukContext
dukCreateHeapDefault =
  dukCreateHeap nullFunPtr nullFunPtr nullFunPtr nullPtr nullFunPtr


{#fun duk_throw as ^ {`CDukContext'} -> `()'#}


{#fun duk_fatal as ^ {`CDukContext', `Int', `String'} -> `()'#}


{#fun duk_is_strict_call as ^ {`CDukContext'} -> `Bool'#}


{#fun duk_is_constructor_call as ^ {`CDukContext'} -> `Bool'#}


{#fun duk_normalize_index as ^ {`CDukContext', `Int'} -> `Int'#}


{#fun duk_require_normalize_index as ^ {`CDukContext', `Int'} -> `Int'#}


{#fun duk_is_valid_index as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_require_valid_index as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_get_top as ^ {`CDukContext'} -> `Int'#}


{#fun duk_set_top as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_get_top_index as ^ {`CDukContext'} -> `Int'#}


{#fun duk_require_top_index as ^ {`CDukContext'} -> `Int'#}


{#fun duk_check_stack as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_require_stack as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_check_stack_top as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_require_stack_top as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_swap as ^ {`CDukContext', `Int', `Int'} -> `()'#}


{#fun duk_swap_top as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_dup as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_dup_top as ^ {`CDukContext'} -> `()'#}


{#fun duk_insert as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_replace as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_copy as ^ {`CDukContext', `Int', `Int'} -> `()'#}


{#fun duk_remove as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_xcopymove_raw as ^ {`CDukContext', `CDukContext', `Int', `Bool'} -> `()'#}


dukXMoveTop :: CDukContext -> CDukContext -> Int -> IO ()
dukXMoveTop c1 c2 count =
  dukXcopymoveRaw c1 c2 count False


dukXCopyTop :: CDukContext -> CDukContext -> Int -> IO ()
dukXCopyTop c1 c2 count =
  dukXcopymoveRaw c1 c2 count True


{#fun duk_push_undefined as ^ {`CDukContext'} -> `()'#}


{#fun duk_push_null as ^ {`CDukContext'} -> `()'#}


{#fun duk_push_boolean as ^ {`CDukContext', `Bool'} -> `()'#}


{#fun duk_push_true as ^ {`CDukContext'} -> `()'#}


{#fun duk_push_false as ^ {`CDukContext'} -> `()'#}


{#fun duk_push_number as ^ {`CDukContext', `Double'} -> `()'#}


{#fun duk_push_nan as ^ {`CDukContext'} -> `()'#}


{#fun duk_push_int as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_push_uint as ^ {`CDukContext', `Word32'} -> `()'#}


{#fun duk_push_string as ^ {`CDukContext', `String'} -> `String'#}


withCStringLenCSize :: String -> ((CString, CSize) -> IO a) -> IO a
withCStringLenCSize s f =
  withCStringLen s $ \(cStr, len) -> f (cStr, fromIntegral len)


{#fun duk_push_lstring as ^ {`CDukContext', withCStringLenCSize* `String'&} -> `String'#}


{#fun duk_push_pointer as ^ {`CDukContext', `Ptr ()'} -> `()'#}


{#fun duk_push_string_file_raw as ^ {`CDukContext', `String', `Word32'} -> `String'#}


{#fun duk_push_this as ^ {`CDukContext'} -> `()'#}


{#fun duk_push_current_function as ^ {`CDukContext'} -> `()'#}


{#fun duk_push_current_thread as ^ {`CDukContext'} -> `()'#}


{#fun duk_push_global_object as ^ {`CDukContext'} -> `()'#}


{#fun duk_push_heap_stash as ^ {`CDukContext'} -> `()'#}


{#fun duk_push_global_stash as ^ {`CDukContext'} -> `()'#}


{#fun duk_push_thread_stash as ^ {`CDukContext', `CDukContext'} -> `()'#}


{#fun duk_push_object as ^ {`CDukContext'} -> `Int'#}


{#fun duk_push_array as ^ {`CDukContext'} -> `Int'#}


-- duk_push_c_function
-- duk_push_c_lightfunc


{#fun duk_push_thread_raw as ^ {`CDukContext', `Word32'} -> `Int'#}


{#fun duk_push_buffer_raw as ^ {`CDukContext', `CSize', `Word16'} -> `Ptr ()'#}


{#fun duk_push_buffer_object as ^ {`CDukContext', `Int', `CSize', `CSize', `Word32'} -> `()'#}


{#fun duk_push_heapptr as ^ {`CDukContext', `Ptr ()'} -> `Int'#}


{#fun duk_pop as ^ {`CDukContext'} -> `()'#}


{#fun duk_pop_n as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_pop_2 as ^ {`CDukContext'} -> `()'#}


{#fun duk_pop_3 as ^ {`CDukContext'} -> `()'#}


{#enum define DukType { DUK_TYPE_NONE as DukTypeNone
                      , DUK_TYPE_UNDEFINED as DukTypeUndefined
                      , DUK_TYPE_NULL as DukTypeNull
                      , DUK_TYPE_BOOLEAN as DukTypeBoolean
                      , DUK_TYPE_NUMBER as DukTypeNumber
                      , DUK_TYPE_STRING as DukTypeString
                      , DUK_TYPE_OBJECT as DukTypeObject
                      , DUK_TYPE_BUFFER as DukTypeBuffer
                      , DUK_TYPE_POINTER as DukTypePointer
                      , DUK_TYPE_LIGHTFUNC as DukTypeLightfunc } deriving (Eq, Show)#}


{#fun duk_get_type as ^ {`CDukContext', `Int'} -> `DukType'#}


{#fun duk_check_type as ^ {`CDukContext', `Int', `DukType'} -> `Bool'#}


newtype DukTypeMask =
  DukTypeMask { unDukTypeMask :: CUInt }


dukTypeMaskNone :: DukTypeMask
dukTypeMaskNone = DukTypeMask 1


dukTypeMaskUndefined :: DukTypeMask
dukTypeMaskUndefined = DukTypeMask 2


dukTypeMaskNull :: DukTypeMask
dukTypeMaskNull = DukTypeMask 4


dukTypeMaskBoolean :: DukTypeMask
dukTypeMaskBoolean = DukTypeMask 8


dukTypeMaskNumber :: DukTypeMask
dukTypeMaskNumber = DukTypeMask 16


dukTypeMaskString :: DukTypeMask
dukTypeMaskString = DukTypeMask 32


dukTypeMaskObject :: DukTypeMask
dukTypeMaskObject = DukTypeMask 64


dukTypeMaskBuffer :: DukTypeMask
dukTypeMaskBuffer = DukTypeMask 128


dukTypeMaskPointer :: DukTypeMask
dukTypeMaskPointer = DukTypeMask 256


dukTypeMaskLightfunc :: DukTypeMask
dukTypeMaskLightfunc = DukTypeMask 512


dukTypeMaskThrow :: DukTypeMask
dukTypeMaskThrow = DukTypeMask 1024


combineDukTypeMask :: [DukTypeMask] -> DukTypeMask
combineDukTypeMask = DukTypeMask . foldl1' (.|.) . map unDukTypeMask


{#fun duk_get_type_mask as ^ {`CDukContext', `Int'} -> `DukTypeMask' DukTypeMask#}


{#fun duk_check_type_mask as ^ {`CDukContext', `Int', unDukTypeMask `DukTypeMask'} -> `Bool'#}


{#fun duk_is_undefined as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_is_null as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_is_null_or_undefined as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_is_boolean as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_is_number as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_is_nan as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_is_string as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_is_object as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_is_buffer as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_is_pointer as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_is_lightfunc as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_is_array as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_is_function as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_is_c_function as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_is_ecmascript_function as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_is_bound_function as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_is_thread as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_is_callable as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_is_dynamic_buffer as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_is_fixed_buffer as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_is_external_buffer as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_get_error_code as ^ {`CDukContext', `Int'} -> `Int'#}


{#fun duk_get_boolean as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_get_number as ^ {`CDukContext', `Int'} -> `Double'#}


{#fun duk_get_int as ^ {`CDukContext', `Int'} -> `Int'#}


{#fun duk_get_uint as ^ {`CDukContext', `Int'} -> `Word32'#}


{#fun duk_get_string as ^ {`CDukContext', `Int'} -> `String'#}


{#fun duk_get_lstring as _dukGetLString {`CDukContext', `Int', alloca- `CSize' peek*} -> `CString'#}


dukGetLString :: CDukContext -> Int -> IO String
dukGetLString ctx idx = do
  (cStr, len) <- _dukGetLString ctx idx
  peekCStringLen (cStr, fromIntegral len)


{#fun duk_get_buffer as ^ {`CDukContext', `Int', alloca- `CSize' peek*} -> `Ptr ()'#}


{#fun duk_get_buffer_data as ^ {`CDukContext', `Int', alloca- `CSize' peek*} -> `Ptr ()'#}


{#fun duk_get_pointer as ^ {`CDukContext', `Int'} -> `Ptr ()'#}


-- duk_get_c_function


{#fun duk_get_context as ^ {`CDukContext', `Int'} -> `CDukContext'#}


{#fun duk_get_heapptr as ^ {`CDukContext', `Int'} -> `Ptr ()'#}


{#fun duk_get_length as ^ {`CDukContext', `Int'} -> `CSize'#}


{#fun duk_require_undefined as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_require_null as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_require_boolean as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_require_number as ^ {`CDukContext', `Int'} -> `Double'#}


{#fun duk_require_int as ^ {`CDukContext', `Int'} -> `Int'#}


{#fun duk_require_uint as ^ {`CDukContext', `Int'} -> `Word32'#}


{#fun duk_require_string as ^ {`CDukContext', `Int'} -> `String'#}


{#fun duk_require_lstring as _dukRequireLString {`CDukContext', `Int', alloca- `CSize' peek*} -> `CString'#}


dukRequireLString :: CDukContext -> Int -> IO String
dukRequireLString ctx idx = do
  (cStr, len) <- _dukRequireLString ctx idx
  peekCStringLen (cStr, fromIntegral len)


{#fun duk_require_buffer as ^ {`CDukContext', `Int', alloca- `CSize' peek*} -> `Ptr ()'#}


{#fun duk_require_buffer_data as ^ {`CDukContext', `Int', alloca- `CSize' peek*} -> `Ptr ()'#}


{#fun duk_require_pointer as ^ {`CDukContext', `Int'} -> `Ptr ()'#}


-- duk_require_c_function


{#fun duk_require_context as ^ {`CDukContext', `Int'} -> `CDukContext'#}


{#fun duk_require_heapptr as ^ {`CDukContext', `Int'} -> `Ptr ()'#}


{#fun duk_to_undefined as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_to_null as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_to_boolean as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_to_number as ^ {`CDukContext', `Int'} -> `Double'#}


{#fun duk_to_int as ^ {`CDukContext', `Int'} -> `Int'#}


{#fun duk_to_uint as ^ {`CDukContext', `Int'} -> `Word32'#}


{#fun duk_to_int32 as ^ {`CDukContext', `Int'} -> `Int32'#}


{#fun duk_to_uint32 as ^ {`CDukContext', `Int'} -> `Word32'#}


{#fun duk_to_uint16 as ^ {`CDukContext', `Int'} -> `Word16'#}


{#fun duk_to_string as ^ {`CDukContext', `Int'} -> `String'#}


{#fun duk_to_lstring as _dukToLString {`CDukContext', `Int', alloca- `CSize' peek*} -> `CString'#}


dukToLString :: CDukContext -> Int -> IO String
dukToLString ctx idx = do
  (cStr, len) <- _dukToLString ctx idx
  peekCStringLen (cStr, fromIntegral len)


-- duk_to_buffer_raw flags


{#fun duk_to_pointer as ^ {`CDukContext', `Int'} -> `Ptr ()'#}


{#fun duk_to_object as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_to_defaultvalue as ^ {`CDukContext', `Int', `Int'} -> `()'#}


{#fun duk_to_primitive as ^ {`CDukContext', `Int', `Int'} -> `()'#}


{#fun duk_safe_to_lstring as _dukSafeToLString {`CDukContext', `Int', alloca- `CSize' peek*} -> `CString'#}


dukSafeToLString :: CDukContext -> Int -> IO String
dukSafeToLString ctx idx = do
  (cStr, len) <- _dukSafeToLString ctx idx
  peekCStringLen (cStr, fromIntegral len)


{#fun duk_base64_encode as ^ {`CDukContext', `Int'} -> `String'#}


{#fun duk_base64_decode as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_hex_encode as ^ {`CDukContext', `Int'} -> `String'#}


{#fun duk_hex_decode as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_json_encode as ^ {`CDukContext', `Int'} -> `String'#}


{#fun duk_json_decode as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_resize_buffer as ^ {`CDukContext', `Int', `CSize'} -> `Ptr ()'#}


{#fun duk_steal_buffer as ^ {`CDukContext', `Int', alloca- `CSize' peek*} -> `Ptr ()'#}


{#fun duk_config_buffer as ^ {`CDukContext', `Int', `Ptr ()', `CSize'} -> `()'#}


{#fun duk_get_prop as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_get_prop_string as ^ {`CDukContext', `Int', `String'} -> `Bool'#}


{#fun duk_get_prop_index as ^ {`CDukContext', `Int', `Int'} -> `Bool'#}


{#fun duk_put_prop as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_put_prop_string as ^ {`CDukContext', `Int', `String'} -> `Bool'#}


{#fun duk_put_prop_index as ^ {`CDukContext', `Int', `Int'} -> `Bool'#}


{#fun duk_del_prop as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_del_prop_string as ^ {`CDukContext', `Int', `String'} -> `Bool'#}


{#fun duk_del_prop_index as ^ {`CDukContext', `Int', `Int'} -> `Bool'#}


{#fun duk_has_prop as ^ {`CDukContext', `Int'} -> `Bool'#}


{#fun duk_has_prop_string as ^ {`CDukContext', `Int', `String'} -> `Bool'#}


{#fun duk_has_prop_index as ^ {`CDukContext', `Int', `Int'} -> `Bool'#}


{#fun duk_get_global_string as ^ {`CDukContext', `String'} -> `Bool'#}


{#fun duk_put_global_string as ^ {`CDukContext', `String'} -> `Bool'#}


{#fun duk_get_prototype as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_set_prototype as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_get_finalizer as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_set_finalizer as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_set_global_object as ^ {`CDukContext'} -> `()'#}


{#fun duk_get_magic as ^ {`CDukContext', `Int'} -> `Int'#}


{#fun duk_set_magic as ^ {`CDukContext', `Int', `Int'} -> `()'#}


{#fun duk_get_current_magic as ^ {`CDukContext'} -> `Int'#}


-- duk_put_function_list
-- duk_put_number_list
-- duk_compact
-- duk_enum
-- duk_next


{#fun duk_concat as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_join as ^ {`CDukContext', `Int'} -> `()'#}


-- duk_decode_string
-- duk_map_string


{#fun duk_substring as ^ {`CDukContext', `Int', `CSize', `CSize'} -> `()'#}


{#fun duk_trim as ^ {`CDukContext', `Int'} -> `()'#}


-- duk_charcode_at


{#fun duk_equals as ^ {`CDukContext', `Int', `Int'} -> `Bool'#}


{#fun duk_strict_equals as ^ {`CDukContext', `Int', `Int'} -> `Bool'#}


{#fun duk_instanceof as ^ {`CDukContext', `Int', `Int'} -> `Bool'#}


{#fun duk_call as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_call_method as ^ {`CDukContext', `Int'} -> `()'#}


{#fun duk_call_prop as ^ {`CDukContext', `Int', `Int'} -> `()'#}


{#fun duk_pcall as ^ {`CDukContext', `Int'} -> `Int'#}


{#fun duk_pcall_method as ^ {`CDukContext', `Int'} -> `Int'#}


{#fun duk_pcall_prop as ^ {`CDukContext', `Int', `Int'} -> `Int'#}


{#fun duk_pnew as ^ {`CDukContext', `Int'} -> `Int'#}


{#fun duk_new as ^ {`CDukContext', `Int'} -> `()'#}


-- duk_safe_call


newtype DukCompileFlags = DukCompileFlags { unDukCompileFlags :: CUInt }


combineDukCompileFlags :: [DukCompileFlags] -> DukCompileFlags
combineDukCompileFlags = DukCompileFlags . foldl1' (.|.) . map unDukCompileFlags


dukCompileEval :: DukCompileFlags
dukCompileEval = DukCompileFlags 1


dukCompileFunction :: DukCompileFlags
dukCompileFunction = DukCompileFlags 2


dukCompileStrict :: DukCompileFlags
dukCompileStrict = DukCompileFlags 4


dukCompileSafe :: DukCompileFlags
dukCompileSafe = DukCompileFlags 8


dukCompileNoResult :: DukCompileFlags
dukCompileNoResult = DukCompileFlags 16


dukCompileNoSource :: DukCompileFlags
dukCompileNoSource = DukCompileFlags 32


dukCompileStrlen :: DukCompileFlags
dukCompileStrlen = DukCompileFlags 64


{#fun duk_eval_raw as ^ {`CDukContext', withCStringLenCSize* `String'&, unDukCompileFlags `DukCompileFlags'} -> `Int'#}


nullPtrMod :: (CString -> IO a) -> IO a
nullPtrMod f = f nullPtr


zeroSize :: (CSize -> IO a) -> IO a
zeroSize f = f $ fromIntegral 0


{#fun duk_eval_raw as dukEvalRawNullStr {`CDukContext', nullPtrMod- `()', zeroSize- `()', unDukCompileFlags `DukCompileFlags'} -> `Int'#}


dukEvalFileNoResult :: CDukContext -> FilePath -> IO ()
dukEvalFileNoResult ctx filename = do
  void $ dukPushStringFileRaw ctx filename 0
  void $ dukPushString ctx filename
  void $ dukEvalRawNullStr ctx $ combineDukCompileFlags [dukCompileEval, dukCompileNoResult]


dukEvalFile :: CDukContext -> FilePath -> IO ()
dukEvalFile ctx filename = do
  void $ dukPushStringFileRaw ctx filename 0
  void $ dukPushString ctx filename
  void $ dukEvalRawNullStr ctx dukCompileEval


dukPEvalFileNoResult :: CDukContext -> FilePath -> IO Int
dukPEvalFileNoResult ctx filename = do
  void $ dukPushStringFileRaw ctx filename 0
  void $ dukPushString ctx filename
  dukEvalRawNullStr ctx $ combineDukCompileFlags [dukCompileEval, dukCompileNoResult, dukCompileSafe]


dukPEvalFile :: CDukContext -> FilePath -> IO Int
dukPEvalFile ctx filename = do
  void $ dukPushStringFileRaw ctx filename 0
  void $ dukPushString ctx filename
  dukEvalRawNullStr ctx $ combineDukCompileFlags [dukCompileEval, dukCompileSafe]


{#fun duk_compile_raw as ^ {`CDukContext', withCStringLenCSize* `String'&, unDukCompileFlags `DukCompileFlags'} -> `Int'#}


{#fun duk_dump_function as ^ {`CDukContext'} -> `()'#}


{#fun duk_load_function as ^ {`CDukContext'} -> `()'#}


-- duk_log


{#fun duk_push_context_dump as ^ {`CDukContext'} -> `()'#}


-- duk_debugger_attach
-- duk_debugger_detach
-- duk_debugger_cooperate