(function(factory) {
  typeof define === "function" && define.amd ? define(factory) : factory();
})(function() {
  "use strict";
  const PUBLIC_VERSION = "5";
  if (typeof window !== "undefined")
    (window.__svelte || (window.__svelte = { v: /* @__PURE__ */ new Set() })).v.add(PUBLIC_VERSION);
  const EACH_ITEM_REACTIVE = 1;
  const EACH_INDEX_REACTIVE = 1 << 1;
  const EACH_ITEM_IMMUTABLE = 1 << 4;
  const TEMPLATE_FRAGMENT = 1;
  const TEMPLATE_USE_IMPORT_NODE = 1 << 1;
  const UNINITIALIZED = Symbol();
  const DEV = false;
  const DERIVED = 1 << 1;
  const EFFECT = 1 << 2;
  const RENDER_EFFECT = 1 << 3;
  const BLOCK_EFFECT = 1 << 4;
  const BRANCH_EFFECT = 1 << 5;
  const ROOT_EFFECT = 1 << 6;
  const BOUNDARY_EFFECT = 1 << 7;
  const UNOWNED = 1 << 8;
  const DISCONNECTED = 1 << 9;
  const CLEAN = 1 << 10;
  const DIRTY = 1 << 11;
  const MAYBE_DIRTY = 1 << 12;
  const INERT = 1 << 13;
  const DESTROYED = 1 << 14;
  const EFFECT_RAN = 1 << 15;
  const EFFECT_TRANSPARENT = 1 << 16;
  const HEAD_EFFECT = 1 << 19;
  const EFFECT_HAS_DERIVED = 1 << 20;
  const LOADING_ATTR_SYMBOL = Symbol("");
  var is_array = Array.isArray;
  var index_of = Array.prototype.indexOf;
  var array_from = Array.from;
  var define_property = Object.defineProperty;
  var get_descriptors = Object.getOwnPropertyDescriptors;
  var get_prototype_of = Object.getPrototypeOf;
  function run_all(arr) {
    for (var i = 0; i < arr.length; i++) {
      arr[i]();
    }
  }
  let micro_tasks = [];
  function run_micro_tasks() {
    var tasks = micro_tasks;
    micro_tasks = [];
    run_all(tasks);
  }
  function queue_micro_task(fn) {
    if (micro_tasks.length === 0) {
      queueMicrotask(run_micro_tasks);
    }
    micro_tasks.push(fn);
  }
  function equals(value) {
    return value === this.v;
  }
  function safe_not_equal(a, b) {
    return a != a ? b == b : a !== b || a !== null && typeof a === "object" || typeof a === "function";
  }
  function safe_equals(value) {
    return !safe_not_equal(value, this.v);
  }
  function effect_update_depth_exceeded() {
    {
      throw new Error(`https://svelte.dev/e/effect_update_depth_exceeded`);
    }
  }
  function state_unsafe_local_read() {
    {
      throw new Error(`https://svelte.dev/e/state_unsafe_local_read`);
    }
  }
  function state_unsafe_mutation() {
    {
      throw new Error(`https://svelte.dev/e/state_unsafe_mutation`);
    }
  }
  let tracing_mode_flag = false;
  function source(v, stack) {
    var signal = {
      f: 0,
      // TODO ideally we could skip this altogether, but it causes type errors
      v,
      reactions: null,
      equals,
      rv: 0,
      wv: 0
    };
    return signal;
  }
  // @__NO_SIDE_EFFECTS__
  function mutable_source(initial_value, immutable = false) {
    const s = source(initial_value);
    if (!immutable) {
      s.equals = safe_equals;
    }
    return s;
  }
  function set(source2, value) {
    if (active_reaction !== null && !untracking && is_runes() && (active_reaction.f & (DERIVED | BLOCK_EFFECT)) !== 0 && // If the source was created locally within the current derived, then
    // we allow the mutation.
    (derived_sources === null || !derived_sources.includes(source2))) {
      state_unsafe_mutation();
    }
    return internal_set(source2, value);
  }
  function internal_set(source2, value) {
    if (!source2.equals(value)) {
      source2.v;
      source2.v = value;
      source2.wv = increment_write_version();
      mark_reactions(source2, DIRTY);
      if (active_effect !== null && (active_effect.f & CLEAN) !== 0 && (active_effect.f & (BRANCH_EFFECT | ROOT_EFFECT)) === 0) {
        if (untracked_writes === null) {
          set_untracked_writes([source2]);
        } else {
          untracked_writes.push(source2);
        }
      }
    }
    return value;
  }
  function mark_reactions(signal, status) {
    var reactions = signal.reactions;
    if (reactions === null) return;
    var length = reactions.length;
    for (var i = 0; i < length; i++) {
      var reaction = reactions[i];
      var flags = reaction.f;
      if ((flags & DIRTY) !== 0) continue;
      set_signal_status(reaction, status);
      if ((flags & (CLEAN | UNOWNED)) !== 0) {
        if ((flags & DERIVED) !== 0) {
          mark_reactions(
            /** @type {Derived} */
            reaction,
            MAYBE_DIRTY
          );
        } else {
          schedule_effect(
            /** @type {Effect} */
            reaction
          );
        }
      }
    }
  }
  // @__NO_SIDE_EFFECTS__
  function derived(fn) {
    var flags = DERIVED | DIRTY;
    var parent_derived = active_reaction !== null && (active_reaction.f & DERIVED) !== 0 ? (
      /** @type {Derived} */
      active_reaction
    ) : null;
    if (active_effect === null || parent_derived !== null && (parent_derived.f & UNOWNED) !== 0) {
      flags |= UNOWNED;
    } else {
      active_effect.f |= EFFECT_HAS_DERIVED;
    }
    const signal = {
      ctx: component_context,
      deps: null,
      effects: null,
      equals,
      f: flags,
      fn,
      reactions: null,
      rv: 0,
      v: (
        /** @type {V} */
        null
      ),
      wv: 0,
      parent: parent_derived ?? active_effect
    };
    return signal;
  }
  // @__NO_SIDE_EFFECTS__
  function derived_safe_equal(fn) {
    const signal = /* @__PURE__ */ derived(fn);
    signal.equals = safe_equals;
    return signal;
  }
  function destroy_derived_effects(derived2) {
    var effects = derived2.effects;
    if (effects !== null) {
      derived2.effects = null;
      for (var i = 0; i < effects.length; i += 1) {
        destroy_effect(
          /** @type {Effect} */
          effects[i]
        );
      }
    }
  }
  function get_derived_parent_effect(derived2) {
    var parent = derived2.parent;
    while (parent !== null) {
      if ((parent.f & DERIVED) === 0) {
        return (
          /** @type {Effect} */
          parent
        );
      }
      parent = parent.parent;
    }
    return null;
  }
  function execute_derived(derived2) {
    var value;
    var prev_active_effect = active_effect;
    set_active_effect(get_derived_parent_effect(derived2));
    {
      try {
        destroy_derived_effects(derived2);
        value = update_reaction(derived2);
      } finally {
        set_active_effect(prev_active_effect);
      }
    }
    return value;
  }
  function update_derived(derived2) {
    var value = execute_derived(derived2);
    var status = (skip_reaction || (derived2.f & UNOWNED) !== 0) && derived2.deps !== null ? MAYBE_DIRTY : CLEAN;
    set_signal_status(derived2, status);
    if (!derived2.equals(value)) {
      derived2.v = value;
      derived2.wv = increment_write_version();
    }
  }
  let hydrating = false;
  var is_firefox;
  var first_child_getter;
  var next_sibling_getter;
  // @__NO_SIDE_EFFECTS__
  function get_first_child(node) {
    return first_child_getter.call(node);
  }
  // @__NO_SIDE_EFFECTS__
  function get_next_sibling(node) {
    return next_sibling_getter.call(node);
  }
  function child(node, is_text) {
    {
      return /* @__PURE__ */ get_first_child(node);
    }
  }
  function first_child(fragment, is_text) {
    {
      var first = (
        /** @type {DocumentFragment} */
        /* @__PURE__ */ get_first_child(
          /** @type {Node} */
          fragment
        )
      );
      if (first instanceof Comment && first.data === "") return /* @__PURE__ */ get_next_sibling(first);
      return first;
    }
  }
  function sibling(node, count = 1, is_text = false) {
    let next_sibling = node;
    while (count--) {
      next_sibling = /** @type {TemplateNode} */
      /* @__PURE__ */ get_next_sibling(next_sibling);
    }
    {
      return next_sibling;
    }
  }
  function clear_text_content(node) {
    node.textContent = "";
  }
  let is_throwing_error = false;
  let is_flushing = false;
  let last_scheduled_effect = null;
  let is_updating_effect = false;
  let queued_root_effects = [];
  let dev_effect_stack = [];
  let active_reaction = null;
  let untracking = false;
  function set_active_reaction(reaction) {
    active_reaction = reaction;
  }
  let active_effect = null;
  function set_active_effect(effect2) {
    active_effect = effect2;
  }
  let derived_sources = null;
  let new_deps = null;
  let skipped_deps = 0;
  let untracked_writes = null;
  function set_untracked_writes(value) {
    untracked_writes = value;
  }
  let write_version = 1;
  let read_version = 0;
  let skip_reaction = false;
  function increment_write_version() {
    return ++write_version;
  }
  function check_dirtiness(reaction) {
    var _a;
    var flags = reaction.f;
    if ((flags & DIRTY) !== 0) {
      return true;
    }
    if ((flags & MAYBE_DIRTY) !== 0) {
      var dependencies = reaction.deps;
      var is_unowned = (flags & UNOWNED) !== 0;
      if (dependencies !== null) {
        var i;
        var dependency;
        var is_disconnected = (flags & DISCONNECTED) !== 0;
        var is_unowned_connected = is_unowned && active_effect !== null && !skip_reaction;
        var length = dependencies.length;
        if (is_disconnected || is_unowned_connected) {
          var derived2 = (
            /** @type {Derived} */
            reaction
          );
          var parent = derived2.parent;
          for (i = 0; i < length; i++) {
            dependency = dependencies[i];
            if (is_disconnected || !((_a = dependency == null ? void 0 : dependency.reactions) == null ? void 0 : _a.includes(derived2))) {
              (dependency.reactions ?? (dependency.reactions = [])).push(derived2);
            }
          }
          if (is_disconnected) {
            derived2.f ^= DISCONNECTED;
          }
          if (is_unowned_connected && parent !== null && (parent.f & UNOWNED) === 0) {
            derived2.f ^= UNOWNED;
          }
        }
        for (i = 0; i < length; i++) {
          dependency = dependencies[i];
          if (check_dirtiness(
            /** @type {Derived} */
            dependency
          )) {
            update_derived(
              /** @type {Derived} */
              dependency
            );
          }
          if (dependency.wv > reaction.wv) {
            return true;
          }
        }
      }
      if (!is_unowned || active_effect !== null && !skip_reaction) {
        set_signal_status(reaction, CLEAN);
      }
    }
    return false;
  }
  function propagate_error(error, effect2) {
    var current = effect2;
    while (current !== null) {
      if ((current.f & BOUNDARY_EFFECT) !== 0) {
        try {
          current.fn(error);
          return;
        } catch {
          current.f ^= BOUNDARY_EFFECT;
        }
      }
      current = current.parent;
    }
    is_throwing_error = false;
    throw error;
  }
  function should_rethrow_error(effect2) {
    return (effect2.f & DESTROYED) === 0 && (effect2.parent === null || (effect2.parent.f & BOUNDARY_EFFECT) === 0);
  }
  function handle_error(error, effect2, previous_effect, component_context2) {
    if (is_throwing_error) {
      if (previous_effect === null) {
        is_throwing_error = false;
      }
      if (should_rethrow_error(effect2)) {
        throw error;
      }
      return;
    }
    if (previous_effect !== null) {
      is_throwing_error = true;
    }
    {
      propagate_error(error, effect2);
      return;
    }
  }
  function schedule_possible_effect_self_invalidation(signal, effect2, root2 = true) {
    var reactions = signal.reactions;
    if (reactions === null) return;
    for (var i = 0; i < reactions.length; i++) {
      var reaction = reactions[i];
      if ((reaction.f & DERIVED) !== 0) {
        schedule_possible_effect_self_invalidation(
          /** @type {Derived} */
          reaction,
          effect2,
          false
        );
      } else if (effect2 === reaction) {
        if (root2) {
          set_signal_status(reaction, DIRTY);
        } else if ((reaction.f & CLEAN) !== 0) {
          set_signal_status(reaction, MAYBE_DIRTY);
        }
        schedule_effect(
          /** @type {Effect} */
          reaction
        );
      }
    }
  }
  function update_reaction(reaction) {
    var _a;
    var previous_deps = new_deps;
    var previous_skipped_deps = skipped_deps;
    var previous_untracked_writes = untracked_writes;
    var previous_reaction = active_reaction;
    var previous_skip_reaction = skip_reaction;
    var prev_derived_sources = derived_sources;
    var previous_component_context = component_context;
    var previous_untracking = untracking;
    var flags = reaction.f;
    new_deps = /** @type {null | Value[]} */
    null;
    skipped_deps = 0;
    untracked_writes = null;
    skip_reaction = (flags & UNOWNED) !== 0 && (untracking || !is_updating_effect || active_reaction === null);
    active_reaction = (flags & (BRANCH_EFFECT | ROOT_EFFECT)) === 0 ? reaction : null;
    derived_sources = null;
    set_component_context(reaction.ctx);
    untracking = false;
    read_version++;
    try {
      var result = (
        /** @type {Function} */
        (0, reaction.fn)()
      );
      var deps = reaction.deps;
      if (new_deps !== null) {
        var i;
        remove_reactions(reaction, skipped_deps);
        if (deps !== null && skipped_deps > 0) {
          deps.length = skipped_deps + new_deps.length;
          for (i = 0; i < new_deps.length; i++) {
            deps[skipped_deps + i] = new_deps[i];
          }
        } else {
          reaction.deps = deps = new_deps;
        }
        if (!skip_reaction) {
          for (i = skipped_deps; i < deps.length; i++) {
            ((_a = deps[i]).reactions ?? (_a.reactions = [])).push(reaction);
          }
        }
      } else if (deps !== null && skipped_deps < deps.length) {
        remove_reactions(reaction, skipped_deps);
        deps.length = skipped_deps;
      }
      if (is_runes() && untracked_writes !== null && !untracking && deps !== null && (reaction.f & (DERIVED | MAYBE_DIRTY | DIRTY)) === 0) {
        for (i = 0; i < /** @type {Source[]} */
        untracked_writes.length; i++) {
          schedule_possible_effect_self_invalidation(
            untracked_writes[i],
            /** @type {Effect} */
            reaction
          );
        }
      }
      if (previous_reaction !== null) {
        read_version++;
      }
      return result;
    } finally {
      new_deps = previous_deps;
      skipped_deps = previous_skipped_deps;
      untracked_writes = previous_untracked_writes;
      active_reaction = previous_reaction;
      skip_reaction = previous_skip_reaction;
      derived_sources = prev_derived_sources;
      set_component_context(previous_component_context);
      untracking = previous_untracking;
    }
  }
  function remove_reaction(signal, dependency) {
    let reactions = dependency.reactions;
    if (reactions !== null) {
      var index2 = index_of.call(reactions, signal);
      if (index2 !== -1) {
        var new_length = reactions.length - 1;
        if (new_length === 0) {
          reactions = dependency.reactions = null;
        } else {
          reactions[index2] = reactions[new_length];
          reactions.pop();
        }
      }
    }
    if (reactions === null && (dependency.f & DERIVED) !== 0 && // Destroying a child effect while updating a parent effect can cause a dependency to appear
    // to be unused, when in fact it is used by the currently-updating parent. Checking `new_deps`
    // allows us to skip the expensive work of disconnecting and immediately reconnecting it
    (new_deps === null || !new_deps.includes(dependency))) {
      set_signal_status(dependency, MAYBE_DIRTY);
      if ((dependency.f & (UNOWNED | DISCONNECTED)) === 0) {
        dependency.f ^= DISCONNECTED;
      }
      destroy_derived_effects(
        /** @type {Derived} **/
        dependency
      );
      remove_reactions(
        /** @type {Derived} **/
        dependency,
        0
      );
    }
  }
  function remove_reactions(signal, start_index) {
    var dependencies = signal.deps;
    if (dependencies === null) return;
    for (var i = start_index; i < dependencies.length; i++) {
      remove_reaction(signal, dependencies[i]);
    }
  }
  function update_effect(effect2) {
    var flags = effect2.f;
    if ((flags & DESTROYED) !== 0) {
      return;
    }
    set_signal_status(effect2, CLEAN);
    var previous_effect = active_effect;
    var previous_component_context = component_context;
    var was_updating_effect = is_updating_effect;
    active_effect = effect2;
    is_updating_effect = true;
    try {
      if ((flags & BLOCK_EFFECT) !== 0) {
        destroy_block_effect_children(effect2);
      } else {
        destroy_effect_children(effect2);
      }
      execute_effect_teardown(effect2);
      var teardown2 = update_reaction(effect2);
      effect2.teardown = typeof teardown2 === "function" ? teardown2 : null;
      effect2.wv = write_version;
      var deps = effect2.deps;
      var dep;
      if (DEV && tracing_mode_flag && (effect2.f & DIRTY) !== 0 && deps !== null) ;
      if (DEV) ;
    } catch (error) {
      handle_error(error, effect2, previous_effect, previous_component_context || effect2.ctx);
    } finally {
      is_updating_effect = was_updating_effect;
      active_effect = previous_effect;
    }
  }
  function infinite_loop_guard() {
    try {
      effect_update_depth_exceeded();
    } catch (error) {
      if (last_scheduled_effect !== null) {
        {
          handle_error(error, last_scheduled_effect, null);
        }
      } else {
        throw error;
      }
    }
  }
  function flush_queued_root_effects() {
    try {
      var flush_count = 0;
      while (queued_root_effects.length > 0) {
        if (flush_count++ > 1e3) {
          infinite_loop_guard();
        }
        var root_effects = queued_root_effects;
        var length = root_effects.length;
        queued_root_effects = [];
        for (var i = 0; i < length; i++) {
          var root2 = root_effects[i];
          if ((root2.f & CLEAN) === 0) {
            root2.f ^= CLEAN;
          }
          var collected_effects = process_effects(root2);
          flush_queued_effects(collected_effects);
        }
      }
    } finally {
      is_flushing = false;
      last_scheduled_effect = null;
    }
  }
  function flush_queued_effects(effects) {
    var length = effects.length;
    if (length === 0) return;
    for (var i = 0; i < length; i++) {
      var effect2 = effects[i];
      if ((effect2.f & (DESTROYED | INERT)) === 0) {
        try {
          if (check_dirtiness(effect2)) {
            update_effect(effect2);
            if (effect2.deps === null && effect2.first === null && effect2.nodes_start === null) {
              if (effect2.teardown === null) {
                unlink_effect(effect2);
              } else {
                effect2.fn = null;
              }
            }
          }
        } catch (error) {
          handle_error(error, effect2, null, effect2.ctx);
        }
      }
    }
  }
  function schedule_effect(signal) {
    if (!is_flushing) {
      is_flushing = true;
      queueMicrotask(flush_queued_root_effects);
    }
    var effect2 = last_scheduled_effect = signal;
    while (effect2.parent !== null) {
      effect2 = effect2.parent;
      var flags = effect2.f;
      if ((flags & (ROOT_EFFECT | BRANCH_EFFECT)) !== 0) {
        if ((flags & CLEAN) === 0) return;
        effect2.f ^= CLEAN;
      }
    }
    queued_root_effects.push(effect2);
  }
  function process_effects(root2) {
    var effects = [];
    var effect2 = root2.first;
    while (effect2 !== null) {
      var flags = effect2.f;
      var is_branch = (flags & BRANCH_EFFECT) !== 0;
      var is_skippable_branch = is_branch && (flags & CLEAN) !== 0;
      if (!is_skippable_branch && (flags & INERT) === 0) {
        if ((flags & EFFECT) !== 0) {
          effects.push(effect2);
        } else if (is_branch) {
          effect2.f ^= CLEAN;
        } else {
          var previous_active_reaction = active_reaction;
          try {
            active_reaction = effect2;
            if (check_dirtiness(effect2)) {
              update_effect(effect2);
            }
          } catch (error) {
            handle_error(error, effect2, null, effect2.ctx);
          } finally {
            active_reaction = previous_active_reaction;
          }
        }
        var child2 = effect2.first;
        if (child2 !== null) {
          effect2 = child2;
          continue;
        }
      }
      var parent = effect2.parent;
      effect2 = effect2.next;
      while (effect2 === null && parent !== null) {
        effect2 = parent.next;
        parent = parent.parent;
      }
    }
    return effects;
  }
  function get(signal) {
    var flags = signal.f;
    var is_derived = (flags & DERIVED) !== 0;
    if (active_reaction !== null && !untracking) {
      if (derived_sources !== null && derived_sources.includes(signal)) {
        state_unsafe_local_read();
      }
      var deps = active_reaction.deps;
      if (signal.rv < read_version) {
        signal.rv = read_version;
        if (new_deps === null && deps !== null && deps[skipped_deps] === signal) {
          skipped_deps++;
        } else if (new_deps === null) {
          new_deps = [signal];
        } else if (!skip_reaction || !new_deps.includes(signal)) {
          new_deps.push(signal);
        }
      }
    } else if (is_derived && /** @type {Derived} */
    signal.deps === null && /** @type {Derived} */
    signal.effects === null) {
      var derived2 = (
        /** @type {Derived} */
        signal
      );
      var parent = derived2.parent;
      if (parent !== null && (parent.f & UNOWNED) === 0) {
        derived2.f ^= UNOWNED;
      }
    }
    if (is_derived) {
      derived2 = /** @type {Derived} */
      signal;
      if (check_dirtiness(derived2)) {
        update_derived(derived2);
      }
    }
    return signal.v;
  }
  function untrack(fn) {
    var previous_untracking = untracking;
    try {
      untracking = true;
      return fn();
    } finally {
      untracking = previous_untracking;
    }
  }
  const STATUS_MASK = -7169;
  function set_signal_status(signal, status) {
    signal.f = signal.f & STATUS_MASK | status;
  }
  function push_effect(effect2, parent_effect) {
    var parent_last = parent_effect.last;
    if (parent_last === null) {
      parent_effect.last = parent_effect.first = effect2;
    } else {
      parent_last.next = effect2;
      effect2.prev = parent_last;
      parent_effect.last = effect2;
    }
  }
  function create_effect(type, fn, sync, push2 = true) {
    var is_root = (type & ROOT_EFFECT) !== 0;
    var parent_effect = active_effect;
    var effect2 = {
      ctx: component_context,
      deps: null,
      nodes_start: null,
      nodes_end: null,
      f: type | DIRTY,
      first: null,
      fn,
      last: null,
      next: null,
      parent: is_root ? null : parent_effect,
      prev: null,
      teardown: null,
      transitions: null,
      wv: 0
    };
    if (sync) {
      try {
        update_effect(effect2);
        effect2.f |= EFFECT_RAN;
      } catch (e) {
        destroy_effect(effect2);
        throw e;
      }
    } else if (fn !== null) {
      schedule_effect(effect2);
    }
    var inert = sync && effect2.deps === null && effect2.first === null && effect2.nodes_start === null && effect2.teardown === null && (effect2.f & (EFFECT_HAS_DERIVED | BOUNDARY_EFFECT)) === 0;
    if (!inert && !is_root && push2) {
      if (parent_effect !== null) {
        push_effect(effect2, parent_effect);
      }
      if (active_reaction !== null && (active_reaction.f & DERIVED) !== 0) {
        var derived2 = (
          /** @type {Derived} */
          active_reaction
        );
        (derived2.effects ?? (derived2.effects = [])).push(effect2);
      }
    }
    return effect2;
  }
  function teardown(fn) {
    const effect2 = create_effect(RENDER_EFFECT, null, false);
    set_signal_status(effect2, CLEAN);
    effect2.teardown = fn;
    return effect2;
  }
  function effect(fn) {
    return create_effect(EFFECT, fn, false);
  }
  function template_effect(fn, thunks = [], d = derived) {
    const deriveds = thunks.map(d);
    const effect2 = () => fn(...deriveds.map(get));
    return block(effect2);
  }
  function block(fn, flags = 0) {
    return create_effect(RENDER_EFFECT | BLOCK_EFFECT | flags, fn, true);
  }
  function branch(fn, push2 = true) {
    return create_effect(RENDER_EFFECT | BRANCH_EFFECT, fn, true, push2);
  }
  function execute_effect_teardown(effect2) {
    var teardown2 = effect2.teardown;
    if (teardown2 !== null) {
      const previous_reaction = active_reaction;
      set_active_reaction(null);
      try {
        teardown2.call(null);
      } finally {
        set_active_reaction(previous_reaction);
      }
    }
  }
  function destroy_effect_children(signal, remove_dom = false) {
    var effect2 = signal.first;
    signal.first = signal.last = null;
    while (effect2 !== null) {
      var next = effect2.next;
      destroy_effect(effect2, remove_dom);
      effect2 = next;
    }
  }
  function destroy_block_effect_children(signal) {
    var effect2 = signal.first;
    while (effect2 !== null) {
      var next = effect2.next;
      if ((effect2.f & BRANCH_EFFECT) === 0) {
        destroy_effect(effect2);
      }
      effect2 = next;
    }
  }
  function destroy_effect(effect2, remove_dom = true) {
    var removed = false;
    if ((remove_dom || (effect2.f & HEAD_EFFECT) !== 0) && effect2.nodes_start !== null) {
      var node = effect2.nodes_start;
      var end = effect2.nodes_end;
      while (node !== null) {
        var next = node === end ? null : (
          /** @type {TemplateNode} */
          /* @__PURE__ */ get_next_sibling(node)
        );
        node.remove();
        node = next;
      }
      removed = true;
    }
    destroy_effect_children(effect2, remove_dom && !removed);
    remove_reactions(effect2, 0);
    set_signal_status(effect2, DESTROYED);
    var transitions = effect2.transitions;
    if (transitions !== null) {
      for (const transition of transitions) {
        transition.stop();
      }
    }
    execute_effect_teardown(effect2);
    var parent = effect2.parent;
    if (parent !== null && parent.first !== null) {
      unlink_effect(effect2);
    }
    effect2.next = effect2.prev = effect2.teardown = effect2.ctx = effect2.deps = effect2.fn = effect2.nodes_start = effect2.nodes_end = null;
  }
  function unlink_effect(effect2) {
    var parent = effect2.parent;
    var prev = effect2.prev;
    var next = effect2.next;
    if (prev !== null) prev.next = next;
    if (next !== null) next.prev = prev;
    if (parent !== null) {
      if (parent.first === effect2) parent.first = next;
      if (parent.last === effect2) parent.last = prev;
    }
  }
  function pause_effect(effect2, callback) {
    var transitions = [];
    pause_children(effect2, transitions, true);
    run_out_transitions(transitions, () => {
      destroy_effect(effect2);
      if (callback) callback();
    });
  }
  function run_out_transitions(transitions, fn) {
    var remaining = transitions.length;
    if (remaining > 0) {
      var check = () => --remaining || fn();
      for (var transition of transitions) {
        transition.out(check);
      }
    } else {
      fn();
    }
  }
  function pause_children(effect2, transitions, local) {
    if ((effect2.f & INERT) !== 0) return;
    effect2.f ^= INERT;
    if (effect2.transitions !== null) {
      for (const transition of effect2.transitions) {
        if (transition.is_global || local) {
          transitions.push(transition);
        }
      }
    }
    var child2 = effect2.first;
    while (child2 !== null) {
      var sibling2 = child2.next;
      var transparent = (child2.f & EFFECT_TRANSPARENT) !== 0 || (child2.f & BRANCH_EFFECT) !== 0;
      pause_children(child2, transitions, transparent ? local : false);
      child2 = sibling2;
    }
  }
  function resume_effect(effect2) {
    resume_children(effect2, true);
  }
  function resume_children(effect2, local) {
    if ((effect2.f & INERT) === 0) return;
    effect2.f ^= INERT;
    if ((effect2.f & CLEAN) === 0) {
      effect2.f ^= CLEAN;
    }
    if (check_dirtiness(effect2)) {
      set_signal_status(effect2, DIRTY);
      schedule_effect(effect2);
    }
    var child2 = effect2.first;
    while (child2 !== null) {
      var sibling2 = child2.next;
      var transparent = (child2.f & EFFECT_TRANSPARENT) !== 0 || (child2.f & BRANCH_EFFECT) !== 0;
      resume_children(child2, transparent ? local : false);
      child2 = sibling2;
    }
    if (effect2.transitions !== null) {
      for (const transition of effect2.transitions) {
        if (transition.is_global || local) {
          transition.in();
        }
      }
    }
  }
  let component_context = null;
  function set_component_context(context) {
    component_context = context;
  }
  function push(props, runes = false, fn) {
    component_context = {
      p: component_context,
      c: null,
      e: null,
      m: false,
      s: props,
      x: null,
      l: null
    };
  }
  function pop(component) {
    const context_stack_item = component_context;
    if (context_stack_item !== null) {
      const component_effects = context_stack_item.e;
      if (component_effects !== null) {
        var previous_effect = active_effect;
        var previous_reaction = active_reaction;
        context_stack_item.e = null;
        try {
          for (var i = 0; i < component_effects.length; i++) {
            var component_effect = component_effects[i];
            set_active_effect(component_effect.effect);
            set_active_reaction(component_effect.reaction);
            effect(component_effect.fn);
          }
        } finally {
          set_active_effect(previous_effect);
          set_active_reaction(previous_reaction);
        }
      }
      component_context = context_stack_item.p;
      context_stack_item.m = true;
    }
    return (
      /** @type {T} */
      {}
    );
  }
  function is_runes() {
    return true;
  }
  function without_reactive_context(fn) {
    var previous_reaction = active_reaction;
    var previous_effect = active_effect;
    set_active_reaction(null);
    set_active_effect(null);
    try {
      return fn();
    } finally {
      set_active_reaction(previous_reaction);
      set_active_effect(previous_effect);
    }
  }
  function create_event(event_name, dom, handler, options = {}) {
    function target_handler(event2) {
      if (!options.capture) {
        handle_event_propagation.call(dom, event2);
      }
      if (!event2.cancelBubble) {
        return without_reactive_context(() => {
          return handler == null ? void 0 : handler.call(this, event2);
        });
      }
    }
    if (event_name.startsWith("pointer") || event_name.startsWith("touch") || event_name === "wheel") {
      queue_micro_task(() => {
        dom.addEventListener(event_name, target_handler, options);
      });
    } else {
      dom.addEventListener(event_name, target_handler, options);
    }
    return target_handler;
  }
  function event(event_name, dom, handler, capture, passive) {
    var options = { capture, passive };
    var target_handler = create_event(event_name, dom, handler, options);
    if (dom === document.body || dom === window || dom === document) {
      teardown(() => {
        dom.removeEventListener(event_name, target_handler, options);
      });
    }
  }
  function handle_event_propagation(event2) {
    var _a;
    var handler_element = this;
    var owner_document = (
      /** @type {Node} */
      handler_element.ownerDocument
    );
    var event_name = event2.type;
    var path = ((_a = event2.composedPath) == null ? void 0 : _a.call(event2)) || [];
    var current_target = (
      /** @type {null | Element} */
      path[0] || event2.target
    );
    var path_idx = 0;
    var handled_at = event2.__root;
    if (handled_at) {
      var at_idx = path.indexOf(handled_at);
      if (at_idx !== -1 && (handler_element === document || handler_element === /** @type {any} */
      window)) {
        event2.__root = handler_element;
        return;
      }
      var handler_idx = path.indexOf(handler_element);
      if (handler_idx === -1) {
        return;
      }
      if (at_idx <= handler_idx) {
        path_idx = at_idx;
      }
    }
    current_target = /** @type {Element} */
    path[path_idx] || event2.target;
    if (current_target === handler_element) return;
    define_property(event2, "currentTarget", {
      configurable: true,
      get() {
        return current_target || owner_document;
      }
    });
    var previous_reaction = active_reaction;
    var previous_effect = active_effect;
    set_active_reaction(null);
    set_active_effect(null);
    try {
      var throw_error;
      var other_errors = [];
      while (current_target !== null) {
        var parent_element = current_target.assignedSlot || current_target.parentNode || /** @type {any} */
        current_target.host || null;
        try {
          var delegated = current_target["__" + event_name];
          if (delegated !== void 0 && (!/** @type {any} */
          current_target.disabled || // DOM could've been updated already by the time this is reached, so we check this as well
          // -> the target could not have been disabled because it emits the event in the first place
          event2.target === current_target)) {
            if (is_array(delegated)) {
              var [fn, ...data] = delegated;
              fn.apply(current_target, [event2, ...data]);
            } else {
              delegated.call(current_target, event2);
            }
          }
        } catch (error) {
          if (throw_error) {
            other_errors.push(error);
          } else {
            throw_error = error;
          }
        }
        if (event2.cancelBubble || parent_element === handler_element || parent_element === null) {
          break;
        }
        current_target = parent_element;
      }
      if (throw_error) {
        for (let error of other_errors) {
          queueMicrotask(() => {
            throw error;
          });
        }
        throw throw_error;
      }
    } finally {
      event2.__root = handler_element;
      delete event2.currentTarget;
      set_active_reaction(previous_reaction);
      set_active_effect(previous_effect);
    }
  }
  function create_fragment_from_html(html) {
    var elem = document.createElement("template");
    elem.innerHTML = html;
    return elem.content;
  }
  function assign_nodes(start, end) {
    var effect2 = (
      /** @type {Effect} */
      active_effect
    );
    if (effect2.nodes_start === null) {
      effect2.nodes_start = start;
      effect2.nodes_end = end;
    }
  }
  // @__NO_SIDE_EFFECTS__
  function template(content, flags) {
    var is_fragment = (flags & TEMPLATE_FRAGMENT) !== 0;
    var use_import_node = (flags & TEMPLATE_USE_IMPORT_NODE) !== 0;
    var node;
    var has_start = !content.startsWith("<!>");
    return () => {
      if (node === void 0) {
        node = create_fragment_from_html(has_start ? content : "<!>" + content);
        if (!is_fragment) node = /** @type {Node} */
        /* @__PURE__ */ get_first_child(node);
      }
      var clone = (
        /** @type {TemplateNode} */
        use_import_node || is_firefox ? document.importNode(node, true) : node.cloneNode(true)
      );
      if (is_fragment) {
        var start = (
          /** @type {TemplateNode} */
          /* @__PURE__ */ get_first_child(clone)
        );
        var end = (
          /** @type {TemplateNode} */
          clone.lastChild
        );
        assign_nodes(start, end);
      } else {
        assign_nodes(clone, clone);
      }
      return clone;
    };
  }
  function append(anchor, dom) {
    if (anchor === null) {
      return;
    }
    anchor.before(
      /** @type {Node} */
      dom
    );
  }
  function set_text(text, value) {
    var str = value == null ? "" : typeof value === "object" ? value + "" : value;
    if (str !== (text.__t ?? (text.__t = text.nodeValue))) {
      text.__t = str;
      text.nodeValue = str + "";
    }
  }
  function if_block(node, fn, elseif = false) {
    var anchor = node;
    var consequent_effect = null;
    var alternate_effect = null;
    var condition = UNINITIALIZED;
    var flags = elseif ? EFFECT_TRANSPARENT : 0;
    var has_branch = false;
    const set_branch = (fn2, flag = true) => {
      has_branch = true;
      update_branch(flag, fn2);
    };
    const update_branch = (new_condition, fn2) => {
      if (condition === (condition = new_condition)) return;
      if (condition) {
        if (consequent_effect) {
          resume_effect(consequent_effect);
        } else if (fn2) {
          consequent_effect = branch(() => fn2(anchor));
        }
        if (alternate_effect) {
          pause_effect(alternate_effect, () => {
            alternate_effect = null;
          });
        }
      } else {
        if (alternate_effect) {
          resume_effect(alternate_effect);
        } else if (fn2) {
          alternate_effect = branch(() => fn2(anchor));
        }
        if (consequent_effect) {
          pause_effect(consequent_effect, () => {
            consequent_effect = null;
          });
        }
      }
    };
    block(() => {
      has_branch = false;
      fn(set_branch);
      if (!has_branch) {
        update_branch(null, null);
      }
    }, flags);
  }
  function index(_, i) {
    return i;
  }
  function pause_effects(state, items, controlled_anchor, items_map) {
    var transitions = [];
    var length = items.length;
    for (var i = 0; i < length; i++) {
      pause_children(items[i].e, transitions, true);
    }
    var is_controlled = length > 0 && transitions.length === 0 && controlled_anchor !== null;
    if (is_controlled) {
      var parent_node = (
        /** @type {Element} */
        /** @type {Element} */
        controlled_anchor.parentNode
      );
      clear_text_content(parent_node);
      parent_node.append(
        /** @type {Element} */
        controlled_anchor
      );
      items_map.clear();
      link(state, items[0].prev, items[length - 1].next);
    }
    run_out_transitions(transitions, () => {
      for (var i2 = 0; i2 < length; i2++) {
        var item = items[i2];
        if (!is_controlled) {
          items_map.delete(item.k);
          link(state, item.prev, item.next);
        }
        destroy_effect(item.e, !is_controlled);
      }
    });
  }
  function each(node, flags, get_collection, get_key, render_fn, fallback_fn = null) {
    var anchor = node;
    var state = { flags, items: /* @__PURE__ */ new Map(), first: null };
    var fallback = null;
    var was_empty = false;
    var each_array = /* @__PURE__ */ derived_safe_equal(() => {
      var collection = get_collection();
      return is_array(collection) ? collection : collection == null ? [] : array_from(collection);
    });
    block(() => {
      var array = get(each_array);
      var length = array.length;
      if (was_empty && length === 0) {
        return;
      }
      was_empty = length === 0;
      {
        reconcile(array, state, anchor, render_fn, flags, get_key, get_collection);
      }
      if (fallback_fn !== null) {
        if (length === 0) {
          if (fallback) {
            resume_effect(fallback);
          } else {
            fallback = branch(() => fallback_fn(anchor));
          }
        } else if (fallback !== null) {
          pause_effect(fallback, () => {
            fallback = null;
          });
        }
      }
      get(each_array);
    });
  }
  function reconcile(array, state, anchor, render_fn, flags, get_key, get_collection) {
    var length = array.length;
    var items = state.items;
    var first = state.first;
    var current = first;
    var seen;
    var prev = null;
    var matched = [];
    var stashed = [];
    var value;
    var key;
    var item;
    var i;
    for (i = 0; i < length; i += 1) {
      value = array[i];
      key = get_key(value, i);
      item = items.get(key);
      if (item === void 0) {
        var child_anchor = current ? (
          /** @type {TemplateNode} */
          current.e.nodes_start
        ) : anchor;
        prev = create_item(
          child_anchor,
          state,
          prev,
          prev === null ? state.first : prev.next,
          value,
          key,
          i,
          render_fn,
          flags,
          get_collection
        );
        items.set(key, prev);
        matched = [];
        stashed = [];
        current = prev.next;
        continue;
      }
      {
        update_item(item, value, i);
      }
      if ((item.e.f & INERT) !== 0) {
        resume_effect(item.e);
      }
      if (item !== current) {
        if (seen !== void 0 && seen.has(item)) {
          if (matched.length < stashed.length) {
            var start = stashed[0];
            var j;
            prev = start.prev;
            var a = matched[0];
            var b = matched[matched.length - 1];
            for (j = 0; j < matched.length; j += 1) {
              move(matched[j], start, anchor);
            }
            for (j = 0; j < stashed.length; j += 1) {
              seen.delete(stashed[j]);
            }
            link(state, a.prev, b.next);
            link(state, prev, a);
            link(state, b, start);
            current = start;
            prev = b;
            i -= 1;
            matched = [];
            stashed = [];
          } else {
            seen.delete(item);
            move(item, current, anchor);
            link(state, item.prev, item.next);
            link(state, item, prev === null ? state.first : prev.next);
            link(state, prev, item);
            prev = item;
          }
          continue;
        }
        matched = [];
        stashed = [];
        while (current !== null && current.k !== key) {
          if ((current.e.f & INERT) === 0) {
            (seen ?? (seen = /* @__PURE__ */ new Set())).add(current);
          }
          stashed.push(current);
          current = current.next;
        }
        if (current === null) {
          continue;
        }
        item = current;
      }
      matched.push(item);
      prev = item;
      current = item.next;
    }
    if (current !== null || seen !== void 0) {
      var to_destroy = seen === void 0 ? [] : array_from(seen);
      while (current !== null) {
        if ((current.e.f & INERT) === 0) {
          to_destroy.push(current);
        }
        current = current.next;
      }
      var destroy_length = to_destroy.length;
      if (destroy_length > 0) {
        var controlled_anchor = null;
        pause_effects(state, to_destroy, controlled_anchor, items);
      }
    }
    active_effect.first = state.first && state.first.e;
    active_effect.last = prev && prev.e;
  }
  function update_item(item, value, index2, type) {
    {
      internal_set(item.v, value);
    }
    {
      item.i = index2;
    }
  }
  function create_item(anchor, state, prev, next, value, key, index2, render_fn, flags, get_collection) {
    var reactive = (flags & EACH_ITEM_REACTIVE) !== 0;
    var mutable = (flags & EACH_ITEM_IMMUTABLE) === 0;
    var v = reactive ? mutable ? /* @__PURE__ */ mutable_source(value) : source(value) : value;
    var i = (flags & EACH_INDEX_REACTIVE) === 0 ? index2 : source(index2);
    var item = {
      i,
      v,
      k: key,
      a: null,
      // @ts-expect-error
      e: null,
      prev,
      next
    };
    try {
      item.e = branch(() => render_fn(anchor, v, i, get_collection), hydrating);
      item.e.prev = prev && prev.e;
      item.e.next = next && next.e;
      if (prev === null) {
        state.first = item;
      } else {
        prev.next = item;
        prev.e.next = item.e;
      }
      if (next !== null) {
        next.prev = item;
        next.e.prev = item.e;
      }
      return item;
    } finally {
    }
  }
  function move(item, next, anchor) {
    var end = item.next ? (
      /** @type {TemplateNode} */
      item.next.e.nodes_start
    ) : anchor;
    var dest = next ? (
      /** @type {TemplateNode} */
      next.e.nodes_start
    ) : anchor;
    var node = (
      /** @type {TemplateNode} */
      item.e.nodes_start
    );
    while (node !== end) {
      var next_node = (
        /** @type {TemplateNode} */
        /* @__PURE__ */ get_next_sibling(node)
      );
      dest.before(node);
      node = next_node;
    }
  }
  function link(state, prev, next) {
    if (prev === null) {
      state.first = next;
    } else {
      prev.next = next;
      prev.e.next = next && next.e;
    }
    if (next !== null) {
      next.prev = prev;
      next.e.prev = prev && prev.e;
    }
  }
  function to_class(value, hash, directives) {
    var classname = value == null ? "" : "" + value;
    return classname === "" ? null : classname;
  }
  function set_class(dom, is_html, value, hash, prev_classes, next_classes) {
    var prev = dom.__className;
    if (prev !== value) {
      var next_class_name = to_class(value);
      {
        if (next_class_name == null) {
          dom.removeAttribute("class");
        } else {
          dom.className = next_class_name;
        }
      }
      dom.__className = value;
    }
    return next_classes;
  }
  function set_value(element, value) {
    var attributes = element.__attributes ?? (element.__attributes = {});
    if (attributes.value === (attributes.value = // treat null and undefined the same for the initial value
    value ?? void 0) || // @ts-expect-error
    // `progress` elements always need their value set when it's `0`
    element.value === value && (value !== 0 || element.nodeName !== "PROGRESS")) {
      return;
    }
    element.value = value ?? "";
  }
  function set_attribute(element, attribute, value, skip_warning) {
    var attributes = element.__attributes ?? (element.__attributes = {});
    if (attributes[attribute] === (attributes[attribute] = value)) return;
    if (attribute === "style" && "__styles" in element) {
      element.__styles = {};
    }
    if (attribute === "loading") {
      element[LOADING_ATTR_SYMBOL] = value;
    }
    if (value == null) {
      element.removeAttribute(attribute);
    } else if (typeof value !== "string" && get_setters(element).includes(attribute)) {
      element[attribute] = value;
    } else {
      element.setAttribute(attribute, value);
    }
  }
  var setters_cache = /* @__PURE__ */ new Map();
  function get_setters(element) {
    var setters = setters_cache.get(element.nodeName);
    if (setters) return setters;
    setters_cache.set(element.nodeName, setters = []);
    var descriptors;
    var proto = element;
    var element_proto = Element.prototype;
    while (element_proto !== proto) {
      descriptors = get_descriptors(proto);
      for (var key in descriptors) {
        if (descriptors[key].set) {
          setters.push(key);
        }
      }
      proto = get_prototype_of(proto);
    }
    return setters;
  }
  function prop(props, key, flags, fallback) {
    var prop_value;
    {
      prop_value = /** @type {V} */
      props[key];
    }
    var fallback_value = (
      /** @type {V} */
      fallback
    );
    var fallback_dirty = true;
    var fallback_used = false;
    var get_fallback = () => {
      fallback_used = true;
      if (fallback_dirty) {
        fallback_dirty = false;
        {
          fallback_value = /** @type {V} */
          fallback;
        }
      }
      return fallback_value;
    };
    var getter;
    {
      getter = () => {
        var value = (
          /** @type {V} */
          props[key]
        );
        if (value === void 0) return get_fallback();
        fallback_dirty = true;
        fallback_used = false;
        return value;
      };
    }
    var from_child = false;
    var inner_current_value = /* @__PURE__ */ mutable_source(prop_value);
    var current_value = /* @__PURE__ */ derived(() => {
      var parent_value = getter();
      var child_value = get(inner_current_value);
      if (from_child) {
        from_child = false;
        return child_value;
      }
      return inner_current_value.v = parent_value;
    });
    return function(value, mutation) {
      if (arguments.length > 0) {
        const new_value = mutation ? get(current_value) : value;
        if (!current_value.equals(new_value)) {
          from_child = true;
          set(inner_current_value, new_value);
          if (fallback_used && fallback_value !== void 0) {
            fallback_value = new_value;
          }
          untrack(() => get(current_value));
        }
        return value;
      }
      return get(current_value);
    };
  }
  var root_2 = /* @__PURE__ */ template(`<small style="color: crimson"> </small>`);
  var root_1 = /* @__PURE__ */ template(`<div><label> </label> <input maxlength="3" type="number" min="0"> <button class="svg-btn"></button></div> <!>`, 1);
  var root = /* @__PURE__ */ template(`<div class="aircraft-config" style="display: flex; align-items: center;"><div class="inputs" style="margin-right: 16px;"><div><button class="svg-btn" aria-label="Save and set as default"></button> <button class="svg-btn" aria-label="Update configuration"></button> <button class="svg-btn" aria-label="Reset configuration change"></button> <button class="svg-btn" aria-label="Delete configuration"></button></div> <!></div> <div style="margin: auto;"><p style="margin: 2px 0; text-align: center;">using <span> </span></p> <div></div></div></div>`);
  function App($$anchor, $$props) {
    push($$props, true);
    const promptConfirm = (message, callback, data) => {
      if (confirm(message)) {
        callback(data);
      }
    };
    const saveAndSetDefaultConfiguration = (config2) => {
      console.log("Saving as default", config2);
    };
    const deleteConfiguration = (config2) => {
      console.log("Deleting config", config2);
    };
    const activeAirline = window.activeAirline || { id: 1, airlineType: "Regular" };
    const seatClasses = ["first", "business", "economy"];
    const gameRules = {
      seatSize: { economy: 1, business: 2.5, first: 6 },
      maxSeatsPerClass: 6
    };
    const viewConsts = {
      seatHeight: { economy: 4, business: 9, first: 11 }
    };
    let config = prop($$props, "config");
    config(config() || {
      isDefault: true,
      isValid: true,
      isWideBody: true,
      name: "Boeing 747",
      economy: 0,
      business: 0,
      first: 0,
      maxCapacity: 218,
      original: { economy: 100, business: 20, first: 40 },
      view: []
    });
    let seats = {
      economy: config().original.economy,
      business: config().original.business,
      first: config().original.first
    };
    function calculateTotalCapacity() {
      return seats.economy + seats.business * gameRules.seatSize.business + seats.first * gameRules.seatSize.first;
    }
    function checkIsValid() {
      const totalCapacity = calculateTotalCapacity();
      if (totalCapacity > config().maxCapacity) return false;
      if (seats.economy > 0 && seats.economy < gameRules.maxSeatsPerClass) return false;
      if (seats.business > 0 && seats.business < gameRules.maxSeatsPerClass) return false;
      if (seats.first > 0 && seats.first < gameRules.maxSeatsPerClass) return false;
      return true;
    }
    function calculateMaxSeatsPerRow(aircraft) {
      let seatsPerRow = 6;
      if (aircraft.maxCapacity <= 60) {
        seatsPerRow = 4;
      } else if (aircraft.maxCapacity <= 90) {
        seatsPerRow = 5;
      } else if (aircraft.maxCapacity >= 440) {
        seatsPerRow = 10;
      } else if (aircraft.maxCapacity >= 359) {
        seatsPerRow = 9;
      } else if (aircraft.isWideBody) {
        seatsPerRow = 8;
      }
      return seatsPerRow;
    }
    function calculatePlaneDimensions(aircraft) {
      const maxSeatsPerRow = calculateMaxSeatsPerRow(aircraft);
      const width = aircraft.maxCapacity / maxSeatsPerRow * (3 + 4) + 30;
      const sections = aircraft.isWideBody ? 3 : 2;
      const seatHeight = 8;
      const aisleHeights = (sections - 1) * 7;
      const height = maxSeatsPerRow * seatHeight + aisleHeights;
      const seatsPerSection = maxSeatsPerRow / sections === 3 ? 2 : Math.floor(maxSeatsPerRow / sections);
      const sectionHeight = seatsPerSection * seatHeight;
      const sectionMiddleHeight = aircraft.isWideBody ? (maxSeatsPerRow - 2 * seatsPerSection) * seatHeight : 0;
      return {
        width,
        height,
        sections,
        sectionHeight,
        sectionMiddleHeight
      };
    }
    function updateConfig() {
      config().economy = seats.economy;
      config().business = seats.business;
      config().first = seats.first;
      config().isValid = checkIsValid();
      config().view = getSeatDistribution();
    }
    function getSeatDistribution() {
      const { sections, sectionHeight, sectionMiddleHeight } = calculatePlaneDimensions(config());
      const distribution = Array.from({ length: sections }, () => ({ first: 0, business: 0, economy: 0 }));
      seatClasses.forEach((seatClass) => {
        const seatsPerSectionRow = Math.floor(sectionHeight / (viewConsts.seatHeight[seatClass] + 2));
        const seatsPerMiddleRow = sectionMiddleHeight > 0 ? Math.floor(sectionMiddleHeight / (viewConsts.seatHeight[seatClass] + 2)) : 0;
        const ratioOfMiddleSeats = seatsPerSectionRow > 0 ? seatsPerMiddleRow / seatsPerSectionRow : 0;
        const totalRatioParts = 2 + (sectionMiddleHeight > 0 ? ratioOfMiddleSeats : 0);
        distribution[0][seatClass] = Math.floor(seats[seatClass] / totalRatioParts);
        distribution[1][seatClass] = Math.floor(seats[seatClass] / totalRatioParts);
        if (sections > 2) {
          distribution[2][seatClass] = Math.max(0, Math.floor(seats[seatClass] - (distribution[0][seatClass] + distribution[1][seatClass])));
        }
      });
      return distribution;
    }
    function getTextColors() {
      const totalCapacity = calculateTotalCapacity();
      const capacityColor = totalCapacity > config().maxCapacity ? "crimson" : "inherit";
      const borderColor = totalCapacity > config().maxCapacity ? "crimson" : totalCapacity === config().maxCapacity ? "forestgreen" : "inherit";
      return { capacityColor, borderColor };
    }
    function resetConfig() {
      seats.economy = config().original.economy;
      seats.business = config().original.business;
      seats.first = config().original.first;
      updateConfig();
    }
    function updateSeat(seatClass, value) {
      seats[seatClass] = Number(value);
      updateConfig();
    }
    function setMaxSeats(seatClass) {
      const totalCapacity = calculateTotalCapacity();
      const remainingSpace = config().maxCapacity - totalCapacity;
      const seatSize = gameRules.seatSize[seatClass];
      const newSeats = Math.floor(remainingSpace / seatSize);
      if (newSeats > 0) {
        seats[seatClass] += newSeats;
        updateConfig();
      }
    }
    function saveConfiguration(configuration) {
      const airlineId = (activeAirline == null ? void 0 : activeAirline.id) ?? 1;
      const url = `airlines/${airlineId}/configurations?modelId=${configuration.model ? configuration.model.id : 0}&configurationId=${configuration.id || 0}&economy=${configuration.economy}&business=${configuration.business}&first=${configuration.first}&isDefault=${configuration.isDefault}`;
      fetch(url, {
        method: "PUT",
        headers: {
          "Content-Type": "application/json; charset=utf-8"
        },
        body: JSON.stringify(configuration)
      }).then((response) => {
        if (!response.ok) {
          throw new Error(`HTTP error! status: ${response.status}`);
        }
        return response.json();
      }).then((result) => {
        console.log(result);
      }).catch((error) => {
        console.error("Error:", error);
      });
    }
    updateConfig();
    var div = root();
    var div_1 = child(div);
    var div_2 = child(div_1);
    var button = child(div_2);
    var button_1 = sibling(button, 2);
    var button_2 = sibling(button_1, 2);
    var button_3 = sibling(button_2, 2);
    var node = sibling(div_2, 2);
    each(node, 17, () => seatClasses, index, ($$anchor2, seatClass) => {
      var fragment = root_1();
      var div_3 = first_child(fragment);
      var label = child(div_3);
      var text = child(label);
      var input = sibling(label, 2);
      var button_4 = sibling(input, 2);
      var node_1 = sibling(div_3, 2);
      {
        var consequent = ($$anchor3) => {
          var small = root_2();
          var text_1 = child(small);
          template_effect(() => set_text(text_1, `Must have at least ${gameRules.maxSeatsPerClass} seats`));
          append($$anchor3, small);
        };
        if_block(node_1, ($$render) => {
          if (seats[get(seatClass)] < gameRules.maxSeatsPerClass && seats[get(seatClass)] !== 0) $$render(consequent);
        });
      }
      template_effect(() => {
        set_attribute(label, "for", `${get(seatClass) ?? ""}Seats`);
        set_text(text, `${get(seatClass) ?? ""}:`);
        set_value(input, seats[get(seatClass)]);
        input.disabled = get(seatClass) === "first" && (activeAirline == null ? void 0 : activeAirline.airlineType) === "ULCC" || get(seatClass) === "business" && (activeAirline == null ? void 0 : activeAirline.airlineType) === "ULCC" || get(seatClass) === "economy" && (activeAirline == null ? void 0 : activeAirline.airlineType) === "Luxury";
        set_attribute(button_4, "aria-label", `Increase ${get(seatClass) ?? ""} seats to max`);
      });
      event("input", input, (e) => updateSeat(get(seatClass), e.currentTarget.value));
      event("click", button_4, () => setMaxSeats(get(seatClass)));
      append($$anchor2, fragment);
    });
    var div_4 = sibling(div_1, 2);
    var p = child(div_4);
    var span = sibling(child(p));
    var text_2 = child(span);
    var div_5 = sibling(p, 2);
    template_effect(
      ($0, $1, $2, $3, $4, $5, $6, $7, $8, $9) => {
        button.disabled = $0;
        set_attribute(button, "style", `filter: ${(config().isDefault ? "" : "grayscale(100)") ?? ""};
               cursor: ${(config().isDefault ? "auto" : "pointer") ?? ""}`);
        button_1.disabled = $1;
        set_attribute(button_1, "style", `filter: ${$2 ?? ""};
               cursor: ${$3 ?? ""}`);
        button_3.disabled = config().isDefault;
        set_attribute(button_3, "style", `filter: ${(config().isDefault ? "grayscale(100)" : "grayscale(0)") ?? ""};
               cursor: ${(config().isDefault ? "auto" : "pointer") ?? ""}`);
        set_attribute(span, "style", `color: ${$4 ?? ""}`);
        set_text(text_2, `${$5 ?? ""} of ${config().maxCapacity ?? ""} capacity`);
        set_class(div_5, 1, `planeDiagram ${(config().isWideBody ? "wide-body" : "") ?? ""}`);
        set_attribute(div_5, "style", `width: ${$6 ?? ""}px;
             height: ${$7 ?? ""}px;
             border: ${$8 ?? ""} solid ${$9 ?? ""}`);
      },
      [
        () => config().isDefault || !checkIsValid(),
        () => !checkIsValid(),
        () => checkIsValid() ? "" : "grayscale(100)",
        () => checkIsValid() ? "pointer" : "auto",
        () => getTextColors().capacityColor,
        calculateTotalCapacity,
        () => calculatePlaneDimensions(config()).width,
        () => calculatePlaneDimensions(config()).height,
        () => !checkIsValid() ? "2px" : "1px",
        () => !checkIsValid() ? "crimson" : "#888"
      ]
    );
    event("click", button, () => promptConfirm("Do you want to save and set this configuration as default?", saveAndSetDefaultConfiguration, config()));
    event("click", button_1, () => promptConfirm(`Do you want to change this configuration? This will affect ${config().airplaneCount || 0} airplane(s)`, saveConfiguration, config()));
    event("click", button_2, resetConfig);
    event("click", button_3, () => promptConfirm(`Do you want to delete this configuration? ${config().airplaneCount || 0} airplane(s) with this configuration will be switched to default configuration`, deleteConfiguration, config()));
    append($$anchor, div);
    pop();
  }
  function initAircraftConfig(options = {}) {
    const {
      target = document.getElementById("modelConfigurationModal"),
      props = {}
    } = options;
    if (!target) {
      throw new Error("Target element not found");
    }
    const component = new App(target, props);
    return component;
  }
  window.initAircraftConfig = initAircraftConfig;
});
//# sourceMappingURL=aircraft-config.umd.js.map
