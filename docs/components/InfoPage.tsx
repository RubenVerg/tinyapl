/** @jsx h */
/** @jsxFrag Fragment */

import { PlannedAlert, PlannedBadge } from './Planned.tsx';
import { Info } from '../types.d.ts';

import { Fragment, h } from '../deps/x/htm.ts';

export interface InfoPageProps {
	info: Info;
}

function InfoPage({ info }: InfoPageProps) {
	return <>
		<h1>{info.planned && <PlannedAlert />}{info.name}</h1>

		{info.planned && <PlannedBadge />}

		<div dangerouslySetInnerHTML={{ __html: info.body }} />
	</>;
}

export default InfoPage;