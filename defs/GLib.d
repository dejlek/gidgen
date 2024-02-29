//!repo GLib-2.0

//# These methods are passed buffers to be filled, which currently has issues
//!set record[IOChannel].method[read][disable] 1
//!set record[IOChannel].method[read_chars][disable] 1
//!set record[MainContext].method[query][disable] 1

//# Not introspectable, but could be implemented manually
//!set function[base64_encode_close][introspectable] 0
//!set function[base64_encode_step][introspectable] 0
//!set function[prefix_error_literal][introspectable] 0
//!set function[strfreev][introspectable] 0
//!set function[unichar_to_utf8][introspectable] 0
//!set record[Variant].function[parse][introspectable] 0

//# Disable ByteArray functions and methods, since this type is handled by a custom range type
//!set record[ByteArray].*function[][disable] 1

//# Disable problematic functions (memory allocation issues)
//!set record[Bytes].constructor[new_take][disable] 1
//!set function[base64_decode_inplace][disable] 1

//# Error conflicts with the base D Error type, rename to ErrorG
//!subtype Error ErrorG

//# Override PollFD type kind to be a simple struct, not Boxed
//!kind PollFD Simple

//# We use native associative arrays, disable HashTable bindings
//!set record[HashTable][disable] 1
//!set record[HashTableIter][disable] 1

//# Fix Dir by specifying g_dir_close as the free function and g_dir_open as the constructor
//!set record[Dir][free-function] g_dir_close
//!set record[Dir].method[close][disable] 1
//!set record[Dir].function[open][introspectable] 1
//!rename record[Dir].function[open] constructor
//!set record[Dir].constructor[open].return-value[][transfer-ownership] full

//# Disable binding of unuseful and problematic structures
//!set record[Hook][disable] 1
//!set record[HookList][disable] 1
//!set record[List][disable] 1
//!set record[SList][disable] 1
//!set record[TrashStack][disable] 1

//# Disable Queue fields
//!set record[Queue].field[head][disable] 1
//!set record[Queue].field[tail][disable] 1

//# Disable TestLogMsg fields
//!set record[TestLogMsg].*field[][disable] 1

//# Disable datalist functions
//!set *function[datalist_*][disable] 1

//# Remove problematic type
//!del union[DoubleIEEE754]

//# Remove problematic type
//!del union[FloatIEEE754]

//# Change IConv from a record to a void* alias
//!del record[IConv]
//!add repository.namespace '<alias name="IConv" c:type="GIConv"><type name="void*" c:type="void*"/></alias>'

//# IOChannel should be opaque
//!set record[IOChannel][opaque] 1

//# Set field callback parameters to out
//!set record[IOFuncs].field[io_read].callback[io_read].parameters.parameter[bytes_read][direction] out
//!set record[IOFuncs].field[io_write].callback[io_write].parameters.parameter[bytes_written][direction] out
//!set record[SourceFuncs].field[prepare].callback[prepare].parameters.parameter[timeout_][direction] out

//# FIXME - This contains a union which isn't currently supported
//!set record[VariantDict][opaque] 1

//# These arrays should have zero-terminated set
//!set function[build_filenamev].parameters.parameter[args].array[][zero-terminated] 1
//!set function[build_pathv].parameters.parameter[args].array[][zero-terminated] 1
//!set function[environ_getenv].parameters.parameter[envp].array[][zero-terminated] 1
//!set function[environ_setenv].parameters.parameter[envp].array[][zero-terminated] 1
//!set function[environ_setenv].return-value.array[][zero-terminated] 1
//!set function[environ_unsetenv].parameters.parameter[envp].array[][zero-terminated] 1
//!set function[environ_unsetenv].return-value.array[][zero-terminated] 1
//!set function[get_environ].return-value.array[][zero-terminated] 1
//!set function[get_filename_charsets].parameters.parameter[filename_charsets].array[][zero-terminated] 1
//!set function[get_language_names].return-value.array[][zero-terminated] 1
//!set function[get_language_names_with_category].return-value.array[][zero-terminated] 1
//!set function[get_locale_variants].return-value.array[][zero-terminated] 1
//!set function[get_system_config_dirs].return-value.array[][zero-terminated] 1
//!set function[get_system_data_dirs].return-value.array[][zero-terminated] 1
//!set function[listenv].return-value.array[][zero-terminated] 1
//!set function[spawn_async].parameters.parameter[argv].array[][zero-terminated] 1
//!set function[spawn_async].parameters.parameter[envp].array[][zero-terminated] 1
//!set function[spawn_async_with_fds].parameters.parameter[argv].array[][zero-terminated] 1
//!set function[spawn_async_with_fds].parameters.parameter[envp].array[][zero-terminated] 1
//!set function[spawn_async_with_pipes_and_fds].parameters.parameter[argv].array[][zero-terminated] 1
//!set function[spawn_async_with_pipes_and_fds].parameters.parameter[envp].array[][zero-terminated] 1
//!set function[spawn_async_with_pipes].parameters.parameter[argv].array[][zero-terminated] 1
//!set function[spawn_async_with_pipes].parameters.parameter[envp].array[][zero-terminated] 1
//!set function[spawn_sync].parameters.parameter[argv].array[][zero-terminated] 1
//!set function[spawn_sync].parameters.parameter[envp].array[][zero-terminated] 1
//!set function[strsplit].return-value.array[][zero-terminated] 1
//!set function[strsplit_set].return-value.array[][zero-terminated] 1
//!set function[str_tokenize_and_fold].return-value.array[][zero-terminated] 1
//!set function[str_tokenize_and_fold].parameters.parameter[ascii_alternates].array[][zero-terminated] 1
//!set record[KeyFile].method[get_groups].return-value.array[][zero-terminated] 1
//!set record[KeyFile].method[get_keys].return-value.array[][zero-terminated] 1
//!set record[KeyFile].method[load_from_dirs].parameters.parameter[search_dirs].array[][zero-terminated] 1
//!set record[MatchInfo].method[fetch_all].return-value.array[][zero-terminated] 1
//!set record[OptionContext].method[add_main_entries].parameters.parameter[entries].array[][zero-terminated] 1
//!set record[OptionContext].method[parse_strv].parameters.parameter[arguments].array[][zero-terminated] 1
//!set record[OptionGroup].method[add_entries].parameters.parameter[entries].array[][zero-terminated] 1
//!set record[StrvBuilder].method[addv].parameters.parameter[value].array[][zero-terminated] 1
//!set record[StrvBuilder].method[end].return-value.array[][zero-terminated] 1
//!set record[Uri].function[list_extract_uris].return-value.array[][zero-terminated] 1
//!set record[Regex].method[split].return-value.array[][zero-terminated] 1
//!set record[Regex].method[split_full].return-value.array[][zero-terminated] 1
//!set record[Regex].function[split_simple].return-value.array[][zero-terminated] 1

//# Change arrays of chars to strings
//!set function[spawn_command_line_sync].parameters.parameter[standard_error].array '<type name="utf8" c:type="char**"/>'
//!set function[spawn_command_line_sync].parameters.parameter[standard_output].array '<type name="utf8" c:type="char**"/>'
//!set function[spawn_sync].parameters.parameter[standard_error].array '<type name="utf8" c:type="char**"/>'
//!set function[spawn_sync].parameters.parameter[standard_output].array '<type name="utf8" c:type="char**"/>'
//!set record[Variant].method[get_bytestring].return-value.array '<type name="string" c:type="const char*"/>'
//!set record[Variant].constructor[new_bytestring].parameters.parameter[string].array '<type name="string" c:type="const char*"/>'

//# Missing array argument length
//!set record[IOChannel].method[write_chars].parameters.parameter[buf].array[][length] 1

//# Add missing free functions
//!set record[AsyncQueue][free-function] g_async_queue_unref
//!set record[Hmac][free-function] g_hmac_unref
//!set record[Hook][free-function] g_hook_free
//!set record[List][free-function] g_list_free
//!set record[OptionContext][free-function] g_option_context_free
//!set record[Queue][free-function] g_queue_free
//!set record[Rand][free-function] g_rand_free
//!set record[Scanner][free-function] g_scanner_destroy
//!set record[Sequence][free-function] g_sequence_free
//!set record[SList][free-function] g_slist_free
//!set record[StringChunk][free-function] g_string_chunk_free
//!set record[StrvBuilder][free-function] g_strv_builder_unref
//!set record[TestCase][free-function] g_test_case_free
//!set record[TestSuite][free-function] g_test_suite_free
//!set record[VariantIter][free-function] g_variant_iter_free

//# Change Variant to a class, set to glib:fundamental, and add ref and unref functions
//!rename record[Variant] class
//!set class[Variant][glib:fundamental] 1
//!set class[Variant][glib:ref-func] g_variant_ref
//!set class[Variant][glib:unref-func] g_variant_unref

//# Disable deprecated g_variant_parser_get_error_quark since it confuses the exception generator
//!set class[Variant].function[parser_get_error_quark][disable] 1

//# Add ref/unref functions to VariantBuilder
//!set record[VariantBuilder][glib:ref-func] g_variant_builder_ref
//!set record[VariantBuilder][glib:unref-func] g_variant_builder_unref
//!set record[VariantBuilder].union[u][private] 1

//# Add missing parameter direction "out"
//!set record[IOChannel].method[read].parameters.parameter[bytes_read][direction] out
//!set record[IOChannel].method[read_line_string].parameters.parameter[terminator_pos][direction] out
//!set record[IOChannel].method[write].parameters.parameter[bytes_written][direction] out

//# Fix string array parameters with lengths to be array of chars not array of strings
//!set record[Regex].method[match_all_full].parameters.parameter[string].array.type[][name] char
//!set record[Regex].method[match_full].parameters.parameter[string].array.type[][name] char
//!set record[Regex].method[replace_literal].parameters.parameter[string].array.type[][name] char
//!set record[Regex].method[replace].parameters.parameter[string].array.type[][name] char
//!set record[Regex].method[split_full].parameters.parameter[string].array.type[][name] char

//# Set proper direction of pointer parameters
//!set function[atomic_int_add].parameters.parameter[atomic][direction] inout
//!set function[atomic_int_and].parameters.parameter[atomic][direction] inout
//!set function[atomic_int_compare_and_exchange_full].parameters.parameter[atomic][direction] inout
//!set function[atomic_int_compare_and_exchange].parameters.parameter[atomic][direction] inout
//!set function[atomic_int_dec_and_test].parameters.parameter[atomic][direction] inout
//!set function[atomic_int_exchange_and_add].parameters.parameter[atomic][direction] inout
//!set function[atomic_int_exchange].parameters.parameter[atomic][direction] inout
//!set function[atomic_int_get].parameters.parameter[atomic][direction] out
//!set function[atomic_int_inc].parameters.parameter[atomic][direction] inout
//!set function[atomic_int_or].parameters.parameter[atomic][direction] inout
//!set function[atomic_int_set].parameters.parameter[atomic][direction] inout
//!set function[atomic_int_xor].parameters.parameter[atomic][direction] inout
//!set function[atomic_ref_count_compare].parameters.parameter[arc][direction] inout
//!set function[atomic_ref_count_dec].parameters.parameter[arc][direction] inout
//!set function[atomic_ref_count_inc].parameters.parameter[arc][direction] inout
//!set function[atomic_ref_count_init].parameters.parameter[arc][direction] inout
//!set function[bit_lock].parameters.parameter[address][direction] inout
//!set function[bit_trylock].parameters.parameter[address][direction] inout
//!set function[bit_unlock].parameters.parameter[address][direction] inout
//!set function[nullify_pointer].parameters.parameter[nullify_location][direction] out
//!set function[ref_count_compare].parameters.parameter[rc][direction] inout
//!set function[ref_count_dec].parameters.parameter[rc][direction] inout
//!set function[ref_count_inc].parameters.parameter[rc][direction] inout
//!set function[ref_count_init].parameters.parameter[rc][direction] inout
//!set record[Timer].method[elapsed].parameters.parameter[microseconds][direction] out

//# Fix or add missing array information
//!set function[assertion_message_cmpstrv].parameters.parameter[arg1].type '<array zero-terminated="1" c:type="const char* const*"><type name="utf8" c:type="const char*"/></array>'
//!set function[assertion_message_cmpstrv].parameters.parameter[arg2].type '<array zero-terminated="1" c:type="const char* const*"><type name="utf8" c:type="const char*"/></array>'
//!set function[slice_get_config_state].return-value.type '<array length="2" zero-terminated="0" c:type="const gint64*"><type name="gint64" c:type="gint64"/></array>'
//!set function[slice_get_config_state].parameters.parameter[n_values][direction] out
//!set function[strjoinv].parameters.parameter[str_array].type '<array zero-terminated="1" c:type="gchar**"><type name="utf8" c:type="gchar*"/></array>'
//!set function[strv_contains].parameters.parameter[strv].type '<array zero-terminated="1" c:type="const gchar* const*"><type name="utf8" c:type="const gchar*"/></array>'
//!set function[strv_equal].parameters.parameter[strv1].type '<array zero-terminated="1" c:type="const gchar* const*"><type name="utf8" c:type="const gchar*"/></array>'
//!set function[strv_equal].parameters.parameter[strv2].type '<array zero-terminated="1" c:type="const gchar* const*"><type name="utf8" c:type="const gchar*"/></array>'
//!set function[strv_length].parameters.parameter[str_array].type '<array zero-terminated="1" c:type="const gchar* const*"><type name="utf8" c:type="const gchar*"/></array>'
//!set function[ucs4_to_utf16].return-value.type '<array zero-terminated="1" c:type="gunichar2*"><type name="guint16" c:type="gunichar2"/></array>'
//!set function[unicode_canonical_decomposition].return-value.type '<array length="1" zero-terminated="0" c:type="gunichar*"><type name="gunichar" c:type="gunichar"/></array>'
//!set function[unicode_canonical_decomposition].parameters.parameter[result_len][direction] out
//!set function[utf16_to_ucs4].return-value.type '<array length="3" zero-terminated="1" c:type="gunichar*"><type name="gunichar" c:type="gunichar"/></array>'
//!set function[utf8_to_ucs4].return-value.type '<array length="3" zero-terminated="1" c:type="gunichar*"><type name="gunichar" c:type="gunichar"/></array>'
//!set function[utf8_to_ucs4_fast].return-value.type '<array length="2" zero-terminated="1" c:type="gunichar*"><type name="gunichar" c:type="gunichar"/></array>'
//!set function[utf8_to_ucs4_fast].parameters.parameter[str].type '<array length="1" zero-terminated="1" c:type="const gchar*"><type name="gchar" c:type="gchar"/></array>'
//!set function[utf8_to_utf16].return-value.type '<array length="3" zero-terminated="1" c:type="gunichar2*"><type name="guint16" c:type="gunichar2"/></array>'
//!set record[Rand].method[set_seed_array].parameters.parameter[seed].type '<array length="1" zero-terminated="0" c:type="const guint32*"><type name="guint32" c:type="guint32"/></array>'
//!set record[TestLogBuffer].method[push].parameters.parameter[bytes].type '<array length="0" zero-terminated="0" c:type="const guint8*"><type name="guint8" c:type="guint8"/></array>'
//!set record[StrvBuilder].method[end].return-value.array '<array zero-terminated="1" c:type="char**"><type name="utf8" c:type="char*"/></array>'

//# Set writable to false as it should be
//!set record[Node].field[children][writable] 0
//!set record[Node].field[next][writable] 0
//!set record[Node].field[parent][writable] 0
//!set record[Node].field[prev][writable] 0
//!set record[Scanner].field[config][writable] 0
//!set record[Scanner].field[next_value][writable] 0
//!set record[Scanner].field[value][writable] 0

//!class global

  /**
  * Template to convert a GHashTable to a D associative array.
  * Params:
  *   K = The key D type
  *   V = The value D type
  *   owned = Set to true if caller takes ownership of hash (frees it), false to leave it alone (default)
  * Returns: The D associative array which is a copy of the data in hash
  */
  V[K] hashTableToMap(K, V, bool owned = false)(GHashTable* hash)
  {
    GHashTableIter iter;
    void* key;
    void* value;
    V[K] map;

    for (g_hash_table_iter_init(&iter, hash); g_hash_table_iter_next(&iter, &key, &value); )
      map[containerGetItem!K(key)] = containerGetItem!V(value);

    static if (owned)
      g_hash_table_unref(hash);

    return map;
  }

//# Use a custom free function since g_thread_pool_free takes 3 arguments
//!set record[ThreadPool][free-function] freePool
//!class ThreadPool

  private static void freePool(GThreadPool* pool)
  {
    g_thread_pool_free(pool, true, false); // immediate, wait
  }
