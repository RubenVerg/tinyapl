/** @jsx h */

import { h } from '../deps/x/htm.ts';

export function PlannedAlert() {
	return <div class='alert alert-warning'>
		This primitive or feature is <strong>planned</strong>, that is, it doesn't appear in the most recent released implementation of TinyAPL. Details may change before it is added!
	</div>
}

export function PlannedBadge() {
	return <span class='badge text-warning border border-warning bg-warning-subtle me-3'>Planned</span>;
}

export function DeprecatedAlert() {
	return <div class='alert alert-danger'>
		This primitive or feature is <strong>deprecated</strong>, meaning it will be removed in a future version of TinyAPL.
	</div>
}

export function DeprecatedBadge() {
	return <span class='badge text-danger border border-danger bg-danger-subtle me-3'>Deprecated</span>
}
